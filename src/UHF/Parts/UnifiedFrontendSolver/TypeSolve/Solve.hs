{-# LANGUAGE ExplicitForAll #-}

module UHF.Parts.UnifiedFrontendSolver.TypeSolve.Solve (solve) where

import UHF.Prelude

import UHF.Data.IR.TypeWithInferVar
import UHF.Parts.UnifiedFrontendSolver.ProgressMade (ProgressMade (..))
import UHF.Parts.UnifiedFrontendSolver.SolveResult (SolveResult (..))
import UHF.Parts.UnifiedFrontendSolver.Solving (SolveMonad, ask_sir, get_type_expr_evaled_as_type, get_value_iden_resolved)
import UHF.Parts.UnifiedFrontendSolver.TypeSolve.Error (ErrorTypeContext (..), Error (..))
import UHF.Parts.UnifiedFrontendSolver.TypeSolve.Misc.SubstituteQuantVar (substitute_quant_var)
import UHF.Parts.UnifiedFrontendSolver.TypeSolve.Task (TypeSolveTask (..), Constraint (..), ExpectInWhat (..))
import UHF.Source.Located (Located(unlocate, Located, just_span))
import UHF.Source.Span (Span)
import qualified Data.Map as Map
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.UnifiedFrontendSolver.Error as UnifiedError (Error(TSError))
import qualified UHF.Util.Arena as Arena

solve :: TypeSolveTask -> SolveMonad (ProgressMade TypeSolveTask)
solve (ConstraintWhenTypeExprEvaledAsType tyeatk make_constraint) = do
    tyeat <- get_type_expr_evaled_as_type tyeatk
    case tyeat of
        Solved tyeat -> pure $ ProgressMade [Constraint $ make_constraint tyeat]
        Inconclusive _ -> pure NoProgressMade
        Errored _ -> pure $ ProgressMade []

solve (Constraint constraint) = do
    solve_constraint constraint >>= \case
        Just (Left e) -> do
            _ <- lift $ lift $ Compiler.tell_error (UnifiedError.TSError e)
            pure $ ProgressMade []
        Just (Right ()) -> pure $ ProgressMade []
        Nothing -> pure NoProgressMade
solve (DefinedToBeTypeOfValueRef ifvk vik) = do
    vi <- get_value_iden_resolved vik
    case vi of
        Solved vi -> pure $ ProgressMade [Constraint $ Expect todo (todo ifvk) (todo vi)]
        Inconclusive _ -> pure NoProgressMade
        Errored _ -> pure $ ProgressMade []

solve (DefinedToBeTypeOfTypeExpr ifvk tyek) = do
    tye <- get_type_expr_evaled_as_type tyek
    case tye of
        Solved tye -> pure $ ProgressMade [Constraint $ Expect todo (todo ifvk) (todo tye)]
        Inconclusive _ -> pure NoProgressMade
        Errored _ -> pure $ ProgressMade []

get_error_type_context :: SolveMonad ErrorTypeContext
get_error_type_context = do
    (_, _, infer_vars) <- get
    (SIR.SIR _ adts type_synonyms qvars _ _) <- ask_sir
    pure (ErrorTypeContext adts type_synonyms qvars infer_vars)

set_infer_var_status :: InferVarKey -> InferVarStatus -> SolveMonad ()
set_infer_var_status infer_var new_status = modify_infer_vars $ \ infer_vars -> Arena.modify infer_vars infer_var (\ (InferVar for _) -> InferVar for new_status)

modify_infer_vars :: (Arena.Arena InferVar InferVarKey -> Arena.Arena InferVar InferVarKey) ->  SolveMonad ()
modify_infer_vars modification = state $ \ (nr_things, ig_things, infer_vars) -> ((), (nr_things, ig_things, modification infer_vars))

data UnifyError
    = Mismatch Type Type
    | OccursCheck InferVarKey Type

type VarSubGenerator = StateT VarSub
type VarSubMap = Map.Map Type.QuantVarKey VarSub
newtype VarSub = VarSub Int deriving Eq
generate_var_sub :: Applicative m => VarSubGenerator m VarSub
generate_var_sub = StateT $ \ cur_var@(VarSub cur_num) -> pure (cur_var, VarSub $ cur_num + 1)
run_var_sub_generator :: Monad m => VarSubGenerator m r -> m r
run_var_sub_generator s = evalStateT s (VarSub 0)

-- TODO: figure out how to gracefully handle errors because the infer vars become ambiguous if they cant be unified
solve_constraint :: Constraint -> SolveMonad (Maybe (Either Error  ()))
solve_constraint (Eq in_what sp a b) =
    run_var_sub_generator (runExceptT (unify (unlocate a, Map.empty) (unlocate b, Map.empty))) >>= \case
        Right () -> pure $ Just $ Right ()

        Left (Mismatch a_part b_part) ->
            get_error_type_context >>= \ context ->
            pure (Just $ Left $ EqError { eq_error_context = context, eq_error_in_what = in_what, eq_error_span = sp, eq_error_a_whole = a, eq_error_b_whole = b, eq_error_a_part = a_part, eq_error_b_part = b_part })

        Left (OccursCheck infer_var ty) ->
            get_error_type_context >>= \ context ->
            pure (Just $ Left $ OccursCheckError context sp infer_var ty)

solve_constraint (Expect in_what got expect) =
    run_var_sub_generator (runExceptT (unify (unlocate got, Map.empty) (expect, Map.empty))) >>= \case
        Right () -> pure $ Just $ Right ()

        Left (Mismatch got_part expect_part) ->
            get_error_type_context >>= \ context ->
            pure (Just $ Left $ ExpectError { expect_error_context = context, expect_error_in_what = in_what, expect_error_got_whole = got, expect_error_expect_whole = expect, expect_error_got_part = got_part, expect_error_expect_part = expect_part })

        Left (OccursCheck infer_var ty) ->
            get_error_type_context >>= \ context ->
            pure (Just $ Left $ OccursCheckError context (just_span got) infer_var ty)

solve_constraint (InferVarIsApplyResult sp infer_var ty arg) =
    apply_ty sp ty arg >>= \case
        Just (Right applied) ->
            run_var_sub_generator (runExceptT (unify_infer_var (infer_var, Map.empty) (applied, Map.empty) False)) >>= \case
                Right () -> pure $ Just $ Right ()

                Left (Mismatch infer_var_part applied_part) ->
                    get_error_type_context >>= \ context ->
                    pure (Just $ Left $ ExpectError { expect_error_context = context, expect_error_in_what = InTypeApplication, expect_error_got_whole = Located sp applied, expect_error_expect_whole = Type'InferVar infer_var, expect_error_got_part = applied_part, expect_error_expect_part = infer_var_part })

                Left (OccursCheck infer_var ty) ->
                    get_error_type_context >>= \ context ->
                    pure (Just $ Left $ OccursCheckError context sp infer_var ty)

        Just (Left e) -> pure $ Just $ Left e
        Nothing -> pure Nothing

solve_constraint (DefinedToBe infer_var ty) =
    run_var_sub_generator (runExceptT (unify_infer_var (infer_var, Map.empty) (ty, Map.empty) False)) >>= \case
        Right () -> pure $ Just $ Right ()

        Left (Mismatch got_part expect_part) ->
            get_error_type_context >>= \ context ->
            pure (Just $ Left $ todo {- TODO: ExpectError { expect_error_context = context, expect_error_in_what = in_what, expect_error_got_whole = got, expect_error_expect_whole = expect, expect_error_got_part = got_part, expect_error_expect_part = expect_part } -})

        Left (OccursCheck infer_var ty) ->
            get_error_type_context >>= \ context ->
            pure (Just $ Left $ todo {- TODO: OccursCheckError context (just_span got) infer_var ty -})

apply_ty :: Span -> Type -> Type -> SolveMonad (Maybe (Either Error Type))
apply_ty sp (Type'InferVar infer_var) arg = do
    (_, _, infer_vars) <- get
    case Arena.get infer_vars infer_var of
        InferVar _ (Substituted sub) -> apply_ty sp sub arg
        InferVar _ Fresh -> pure Nothing
apply_ty sp ty@(Type'ADT adt params_already_applied) arg = do
    SIR.SIR _ adts _ _ _ _ <- ask_sir
    let (Type.ADT _ _ type_params _) = Arena.get adts adt
    if length params_already_applied < length type_params -- TODO: check kind of arg when higher kinds are implemented
          then pure $ Just $ Right $ Type'ADT adt (params_already_applied <> [arg])
          else Just <$> (Left <$> (DoesNotTakeTypeArgument <$> get_error_type_context <*> pure sp <*> pure ty))
apply_ty sp ty@(Type'Synonym _) _ = Just <$> (Left <$> (DoesNotTakeTypeArgument <$> get_error_type_context <*> pure sp <*> pure ty)) -- TODO: type synonyms with arguments, also TODO: make sure that type synonyms are always fully applied
apply_ty sp ty@Type'Int _ = Just <$> (Left <$> (DoesNotTakeTypeArgument <$> get_error_type_context <*> pure sp <*> pure ty))
apply_ty sp ty@Type'Float _ = Just <$> (Left <$> (DoesNotTakeTypeArgument <$> get_error_type_context <*> pure sp <*> pure ty))
apply_ty sp ty@Type'Char _ = Just <$> (Left <$> (DoesNotTakeTypeArgument <$> get_error_type_context <*> pure sp <*> pure ty))
apply_ty sp ty@Type'String _ = Just <$> (Left <$> (DoesNotTakeTypeArgument <$> get_error_type_context <*> pure sp <*> pure ty))
apply_ty sp ty@Type'Bool _ = Just <$> (Left <$> (DoesNotTakeTypeArgument <$> get_error_type_context <*> pure sp <*> pure ty))
apply_ty sp ty@(Type'Function _ _) _ = Just <$> (Left <$> (DoesNotTakeTypeArgument <$> get_error_type_context <*> pure sp <*> pure ty))
apply_ty sp ty@(Type'Tuple _ _) _ = Just <$> (Left <$> (DoesNotTakeTypeArgument <$> get_error_type_context <*> pure sp <*> pure ty))
apply_ty sp ty@(Type'QuantVar _) _ = Just <$> (Left <$> (DoesNotTakeTypeArgument <$> get_error_type_context <*> pure sp <*> pure ty)) -- TODO: higher kinded variables
apply_ty _ (Type'Forall (first_var :| more_vars) ty) arg =
    -- TODO: check kind of first_var when higher kinded variables are implemented
    case more_vars of
        [] ->
            Just . Right <$> substitute_quant_var first_var arg ty
        more_1:more_more ->
            Just . Right . Type'Forall (more_1 :| more_more) <$> substitute_quant_var first_var arg ty

apply_ty sp ty@Type'Kind'Type _ = Just <$> (Left <$> (DoesNotTakeTypeArgument <$> get_error_type_context <*> pure sp <*> pure ty))
apply_ty sp ty@(Type'Kind'Arrow _ _) _ = Just <$> (Left <$> (DoesNotTakeTypeArgument <$> get_error_type_context <*> pure sp <*> pure ty))
apply_ty sp ty@Type'Kind'Kind _ = Just <$> (Left <$> (DoesNotTakeTypeArgument <$> get_error_type_context <*> pure sp <*> pure ty))

unify :: (Type, VarSubMap) -> (Type, VarSubMap) -> ExceptT UnifyError (VarSubGenerator SolveMonad) ()
unify (Type'InferVar a, a_var_map) b = unify_infer_var (a, a_var_map) b False
unify a (Type'InferVar b, b_var_map) = unify_infer_var (b, b_var_map) a True

unify (Type'ADT a_adt_key a_params, a_var_map) (Type'ADT b_adt_key b_params, b_var_map)
    | a_adt_key == b_adt_key
        && length a_params == length b_params =
        mapM_
            (\ (a_param, b_param) -> unify (a_param, a_var_map) (b_param, b_var_map))
            (zip a_params b_params)

unify (Type'Synonym a_syn_key, a_var_map) b =
    lift (lift (todo )) >>= \ (_, _, get_type_synonym, _) -> -- TODO: ask
    lift (lift $ lift $ get_type_synonym a_syn_key) >>= \ (Type.TypeSynonym _ _ (_, a_expansion)) ->
    unify (a_expansion, a_var_map) b

unify a (Type'Synonym b_syn_key, b_var_map) = do
    (SIR.SIR _ adts type_synonyms qvars _ _) <- lift $ lift $ ask_sir
    let (Type.TypeSynonym _ _ (_, b_expansion)) = Arena.get type_synonyms b_syn_key
    b_expansion <- lift $ lift $ todo b_expansion

    unify a (b_expansion, b_var_map)

unify (Type'Int, _) (Type'Int, _) = pure ()
unify (Type'Float, _) (Type'Float, _) = pure ()
unify (Type'Char, _) (Type'Char, _) = pure ()
unify (Type'String, _) (Type'String, _) = pure ()
unify (Type'Bool, _) (Type'Bool, _) = pure ()
unify (Type'Function a1 r1, var_map_1) (Type'Function a2 r2, var_map_2) = unify (a1, var_map_1) (a2, var_map_2) >> unify (r1, var_map_1) (r2, var_map_2)
unify (Type'Tuple a1 b1, var_map_1) (Type'Tuple a2 b2, var_map_2) = unify (a1, var_map_1) (a2, var_map_2) >> unify (b1, var_map_1) (b2, var_map_2)
-- variables are carefully constructed to be unique for every forall
-- for example if '#(T)' appears twice in a source file then both T's are created twice and have different keys in the type var arena so that each one unifies with itself but not with the other
-- implicitly generated expressions that contain #(...) (for example the constructor functions of adts) also create new variables
unify (a@(Type'QuantVar v1), var_map_1) (b@(Type'QuantVar v2), var_map_2)
    | v1 == v2 = pure ()
    | otherwise =
        let var_1 = Map.lookup v1 var_map_1
            var_2 = Map.lookup v2 var_map_2
        in case (var_1, var_2) of
            (Just var_1, Just var_2)
                | var_1 == var_2 -> pure ()

            _ -> ExceptT (pure $ Left $ Mismatch a b)

unify (Type'Forall vars1 t1, var_map_1) (Type'Forall vars2 t2, var_map_2) = go (toList vars1) t1 var_map_1 (toList vars2) t2 var_map_2
    where
        go (var1:vars1) t1 map1 (var2:vars2) t2 map2 =
            lift generate_var_sub >>= \ new_var_sub ->
            -- this will error if the same type variable appears twice in nested foralls
            -- i.e. something like #(T) T -> #(T) T -> T
            -- because variables are constructed to be unique to each forall, this should never error in practice
            -- TODO: maybe this should not be like this?
            let map1' = Map.insertWith (\ _ _ -> error "variable substitution already in map") var1 new_var_sub map1
                map2' = Map.insertWith (\ _ _ -> error "variable substitution already in map") var2 new_var_sub map2
            in go vars1 t1 map1' vars2 t2 map2'

        go vars1 t1 map1 vars2 t2 map2 = unify (remake_forall_ty vars1 t1, map1) (remake_forall_ty vars2 t2, map2)

        remake_forall_ty [] t1 = t1
        remake_forall_ty (v1:vmore) t1 = Type'Forall (v1 :| vmore) t1

unify (Type'Kind'Type, _) (Type'Kind'Type, _) = pure ()
unify (Type'Kind'Arrow a1 b1, var_map_1) (Type'Kind'Arrow a2 b2, var_map_2) = unify (a1, var_map_1) (a2, var_map_2) >> unify (b1, var_map_1) (b2, var_map_2)
unify (Type'Kind'Kind, _) (Type'Kind'Kind, _) = pure ()

unify (a, _) (b, _) = ExceptT (pure $ Left $ Mismatch a b)

unify_infer_var :: (InferVarKey, VarSubMap) -> (Type, VarSubMap) -> Bool -> ExceptT UnifyError (VarSubGenerator SolveMonad) ()
unify_infer_var (infer_var, infer_var_var_map) (other, other_var_map) infer_var_on_right = do
    (_, _, infer_vars) <- lift $ lift get
    case Arena.get infer_vars infer_var of
        -- if this infer_varnown can be expanded, unify its expansion
        InferVar _ (Substituted infer_var_sub) ->
            if infer_var_on_right
                then unify (other, other_var_map) (infer_var_sub, infer_var_var_map)
                else unify (infer_var_sub, infer_var_var_map) (other, other_var_map)

        -- if this infer_varnown has no substitution, what happens depends on the other type
        InferVar _ Fresh ->
            case other of
                Type'InferVar other_infer_var ->
                    case Arena.get infer_vars other_infer_var of
                        -- if the other type is a substituted infer_varnown, unify this infer_varnown with the other's expansion
                        InferVar _ (Substituted other_infer_var_sub) -> unify_infer_var (infer_var, infer_var_var_map) (other_infer_var_sub, other_var_map) infer_var_on_right

                        -- if the other type is a fresh infer_varnown, both of them are fresh infer_varnowns and the only thing that can be done is to unify them
                        InferVar _ Fresh ->
                            when (infer_var /= other_infer_var) $
                                lift (lift $ set_infer_var_status infer_var (Substituted other))

                -- if the other type is a type and not an infer_varnown
                _ -> lift (lift $ occurs_check infer_var other) >>= \case
                    True -> ExceptT (pure $ Left $ OccursCheck infer_var other)
                    False -> lift (lift $ set_infer_var_status infer_var (Substituted other))

occurs_check :: InferVarKey -> Type -> SolveMonad Bool
-- does the infer var ifv occur anywhere in the type ty?
occurs_check ifv (Type'InferVar other_v) =
    if ifv == other_v
        then pure True
        else do
            (_, _, infer_vars) <- get
            case Arena.get infer_vars other_v of
                InferVar _ (Substituted other_sub) -> occurs_check ifv other_sub
                InferVar _ Fresh -> pure False

occurs_check u (Type'ADT _ params) = or <$> mapM (occurs_check u) params

occurs_check u (Type'Synonym syn_key) =
    -- TODO: reconsider if this is correct
    todo >>= \ (_, _, get_type_synonym, _) ->
    lift (get_type_synonym syn_key) >>= \ (Type.TypeSynonym _ _ (_, other_expansion)) ->
    occurs_check u other_expansion

occurs_check _ Type'Int = pure False
occurs_check _ Type'Float = pure False
occurs_check _ Type'Char = pure False
occurs_check _ Type'String = pure False
occurs_check _ Type'Bool = pure False
occurs_check u (Type'Function a r) = (||) <$> occurs_check u a <*> occurs_check u r
occurs_check u (Type'Tuple a b) = (||) <$> occurs_check u a <*> occurs_check u b
occurs_check _ (Type'QuantVar _) = pure False
occurs_check u (Type'Forall _ ty) = occurs_check u ty
occurs_check _ Type'Kind'Type = pure False
occurs_check u (Type'Kind'Arrow a b) = (||) <$> occurs_check u a <*> occurs_check u b
occurs_check _ Type'Kind'Kind = pure False
