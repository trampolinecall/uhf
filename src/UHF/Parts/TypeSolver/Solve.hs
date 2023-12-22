{-# LANGUAGE ScopedTypeVariables #-}
module UHF.Parts.TypeSolver.Solve
    ( solve -- TODO: remove?

    , apply_type

    , solve_constraint
    , solve_constraint_backlog
    ) where

import UHF.Prelude

import qualified Data.Map as Map

import UHF.Parts.TypeSolver.Constraint
import UHF.Parts.TypeSolver.SolveError
import UHF.Parts.TypeSolver.TypeWithInferVar
import UHF.Parts.TypeSolver.Utils
import UHF.Source.Located (Located (..))
import UHF.Source.Span (Span)
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Parts.TypeSolver.SolveMonad as SolveMonad
import qualified UHF.Util.Arena as Arena

-- e_t for error type - the type of the type expressions in the things that will be passed to error messages
type TypeContextReader e_t t under = ReaderT (Arena.Arena (Type.ADT e_t) Type.ADTKey, Arena.Arena (Type.TypeSynonym e_t) Type.TypeSynonymKey, Type.TypeSynonymKey -> SolveMonad.SolveMonad under (Type.TypeSynonym (t, Type)), Arena.Arena Type.QuantVar Type.QuantVarKey) (SolveMonad.SolveMonad under)

-- TODO: figure out a better way to put this
apply_type :: Monad under => Arena.Arena (Type.ADT e_t) Type.ADTKey -> Arena.Arena (Type.TypeSynonym e_t) Type.TypeSynonymKey -> (Type.TypeSynonymKey -> SolveMonad.SolveMonad under (Type.TypeSynonym (t, Type))) -> Arena.Arena Type.QuantVar Type.QuantVarKey -> InferVarForWhat -> Span -> Type -> Type -> SolveMonad.SolveMonad under Type
apply_type adts type_synonyms get_type_synonym quant_vars for_what sp ty arg =
    SolveMonad.new_infer_var for_what >>= \ result_ifv ->
    solve_constraint adts type_synonyms get_type_synonym quant_vars (InferVarIsApplyResult sp result_ifv ty arg) >>= \ _ -> -- TODO: do not ignore result and push constraint onto backlog if not solvable
    pure (Type'InferVar result_ifv)

-- TODO: figure out a better place in this module to put these 2 functions
solve_constraint :: forall e_t under t. Monad under => Arena.Arena (Type.ADT e_t) Type.ADTKey -> Arena.Arena (Type.TypeSynonym e_t) Type.TypeSynonymKey -> (Type.TypeSynonymKey -> SolveMonad.SolveMonad under (Type.TypeSynonym (t, Type))) -> Arena.Arena Type.QuantVar Type.QuantVarKey -> Constraint -> SolveMonad.SolveMonad under (Maybe (Either (SolveError e_t) ()))
solve_constraint adts type_synonyms get_type_synonym quant_vars constraint = solve adts type_synonyms get_type_synonym quant_vars constraint

solve_constraint_backlog :: Monad under => Arena.Arena (Type.ADT e_t) Type.ADTKey -> Arena.Arena (Type.TypeSynonym e_t) Type.TypeSynonymKey -> (Type.TypeSynonymKey -> SolveMonad.SolveMonad under (Type.TypeSynonym (t, Type))) -> Arena.Arena Type.QuantVar Type.QuantVarKey -> SolveMonad.SolveMonad under ([SolveError e_t], Bool)
solve_constraint_backlog adts type_synonyms get_type_synonym quant_vars = do
    backlog <- SolveMonad.take_backlog
    (new_backlog, errors) <- runWriterT $ go backlog
    SolveMonad.put_backlog new_backlog
    pure (errors, length backlog /= length new_backlog)
    where
        go constraints = do
            next <- constraints
                -- try to solve each constraint and save each one that couldn't be solved in this round
                & mapM (\ constraint ->
                    lift (solve adts type_synonyms get_type_synonym quant_vars constraint) >>= \case
                        Just (Right ()) -> pure Nothing
                        Just (Left err) -> do
                            tell [err]
                            pure Nothing
                        Nothing -> pure (Just constraint)
                )
                & fmap catMaybes

            if length next == 0
                then pure []
                else if length constraints == length next
                    then pure next -- all constraints were deferred, so no more solving can be done
                    else go next

get_error_type_context :: Monad under => TypeContextReader e_t t under (ErrorTypeContext e_t)
get_error_type_context =
    lift SolveMonad.get_infer_vars >>= \ infer_vars ->
    ask >>= \ (adts, type_synonyms, _, vars) ->
    pure (ErrorTypeContext adts type_synonyms vars infer_vars)

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

solve :: Monad under => Arena.Arena (Type.ADT e_t) Type.ADTKey -> Arena.Arena (Type.TypeSynonym e_t) Type.TypeSynonymKey -> (Type.TypeSynonymKey -> SolveMonad.SolveMonad under (Type.TypeSynonym (t, Type))) -> Arena.Arena Type.QuantVar Type.QuantVarKey -> Constraint -> SolveMonad.SolveMonad under (Maybe (Either (SolveError e_t) ()))
solve adts type_synonyms get_type_synonym vars constraints = runReaderT (solve' constraints) (adts, type_synonyms, get_type_synonym, vars)

-- TODO: figure out how to gracefully handle errors because the infer vars become ambiguous if they cant be unified
solve' :: Monad under => Constraint -> TypeContextReader e_t t under (Maybe (Either (SolveError e_t) ()))
solve' (Eq in_what sp a b) =
    run_var_sub_generator (runExceptT (unify (unlocate a, Map.empty) (unlocate b, Map.empty))) >>= \case
        Right () -> pure $ Just $ Right ()

        Left (Mismatch a_part b_part) ->
            get_error_type_context >>= \ context ->
            pure (Just $ Left $ EqError { eq_error_context = context, eq_error_in_what = in_what, eq_error_span = sp, eq_error_a_whole = a, eq_error_b_whole = b, eq_error_a_part = a_part, eq_error_b_part = b_part })

        Left (OccursCheck infer_var ty) ->
            get_error_type_context >>= \ context ->
            pure (Just $ Left $ OccursCheckError context sp infer_var ty)

solve' (Expect in_what got expect) =
    run_var_sub_generator (runExceptT (unify (unlocate got, Map.empty) (expect, Map.empty))) >>= \case
        Right () -> pure $ Just $ Right ()

        Left (Mismatch got_part expect_part) ->
            get_error_type_context >>= \ context ->
            pure (Just $ Left $ ExpectError { expect_error_context = context, expect_error_in_what = in_what, expect_error_got_whole = got, expect_error_expect_whole = expect, expect_error_got_part = got_part, expect_error_expect_part = expect_part })

        Left (OccursCheck infer_var ty) ->
            get_error_type_context >>= \ context ->
            pure (Just $ Left $ OccursCheckError context (just_span got) infer_var ty)

solve' (InferVarIsApplyResult sp infer_var ty arg) =
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

apply_ty :: Monad under => Span -> Type -> Type -> TypeContextReader e_t t under (Maybe (Either (SolveError e_t) Type))
apply_ty sp (Type'InferVar infer_var) arg =
    Arena.get <$> lift SolveMonad.get_infer_vars <*> pure infer_var >>= \case
        InferVar _ (Substituted sub) -> apply_ty sp sub arg
        InferVar _ Fresh -> pure Nothing
apply_ty sp ty@(Type'ADT adt params_already_applied) arg = do
    (adts, _, _, _) <- ask
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
            Just . Right <$> lift (substitute_quant_var first_var arg ty)
        more_1:more_more ->
            Just . Right . Type'Forall (more_1 :| more_more) <$> lift (substitute_quant_var first_var arg ty)

unify :: Monad under => (Type, VarSubMap) -> (Type, VarSubMap) -> ExceptT UnifyError (VarSubGenerator (TypeContextReader e_t t under)) ()
unify (Type'InferVar a, a_var_map) b = unify_infer_var (a, a_var_map) b False
unify a (Type'InferVar b, b_var_map) = unify_infer_var (b, b_var_map) a True

unify (Type'ADT a_adt_key a_params, a_var_map) (Type'ADT b_adt_key b_params, b_var_map)
    | a_adt_key == b_adt_key
        && length a_params == length b_params =
        mapM_
            (\ (a_param, b_param) -> unify (a_param, a_var_map) (b_param, b_var_map))
            (zip a_params b_params )

unify (Type'Synonym a_syn_key, a_var_map) b =
    lift (lift ask) >>= \ (_, _, get_type_synonym, _) ->
    lift (lift $ lift $ get_type_synonym a_syn_key) >>= \ (Type.TypeSynonym _ _ (_, a_expansion)) ->
    unify (a_expansion, a_var_map) b

unify a (Type'Synonym b_syn_key, b_var_map) =
    lift (lift ask) >>= \ (_, _, get_type_synonym, _) ->
    lift (lift $ lift $ get_type_synonym b_syn_key) >>= \ (Type.TypeSynonym _ _ (_, b_expansion)) ->
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
unify (a, _) (b, _) = ExceptT (pure $ Left $ Mismatch a b)

set_infer_var_status :: Monad under => InferVarKey -> InferVarStatus -> TypeContextReader e_t t under ()
set_infer_var_status infer_var new_status = lift $ SolveMonad.modify_infer_vars $ \ ty_arena -> Arena.modify ty_arena infer_var (\ (InferVar for _) -> InferVar for new_status)

unify_infer_var :: Monad under => (InferVarKey, VarSubMap) -> (Type, VarSubMap) -> Bool -> ExceptT UnifyError (VarSubGenerator (TypeContextReader e_t t under)) ()
unify_infer_var (infer_var, infer_var_var_map) (other, other_var_map) infer_var_on_right = Arena.get <$> lift (lift $ lift SolveMonad.get_infer_vars) <*> pure infer_var >>= \case
    -- if this infer_varnown can be expanded, unify its expansion
    InferVar _ (Substituted infer_var_sub) ->
        if infer_var_on_right
            then unify (other, other_var_map) (infer_var_sub, infer_var_var_map)
            else unify (infer_var_sub, infer_var_var_map) (other, other_var_map)

    -- if this infer_varnown has no substitution, what happens depends on the other type
    InferVar _ Fresh ->
        case other of
            Type'InferVar other_infer_var ->
                Arena.get <$> lift (lift $ lift SolveMonad.get_infer_vars) <*> pure other_infer_var >>= \case
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

occurs_check :: Monad under => InferVarKey -> Type -> TypeContextReader e_t t under Bool
-- does the infer var ifv occur anywhere in the type ty?
occurs_check ifv (Type'InferVar other_v) =
    if ifv == other_v
        then pure True
        else
            Arena.get <$> lift SolveMonad.get_infer_vars <*> pure other_v >>= \case
                InferVar _ (Substituted other_sub) -> occurs_check ifv other_sub
                InferVar _ Fresh -> pure False

occurs_check u (Type'ADT _ params) = or <$> mapM (occurs_check u) params

occurs_check u (Type'Synonym syn_key) =
    -- TODO: reconsider if this is correct
    ask >>= \ (_, _, get_type_synonym, _) ->
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
