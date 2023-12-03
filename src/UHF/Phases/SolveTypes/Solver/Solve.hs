module UHF.Phases.SolveTypes.Solver.Solve (solve) where

import UHF.Prelude

import qualified Data.Map as Map

import UHF.Phases.SolveTypes.Aliases
import UHF.Phases.SolveTypes.Error
import UHF.Phases.SolveTypes.Solver.Constraint
import UHF.Phases.SolveTypes.Solver.TypeWithInferVar
import UHF.Phases.SolveTypes.StateWithInferVars
import UHF.Phases.SolveTypes.Utils
import UHF.Source.Located (Located (..))
import UHF.Source.Span (Span)
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Util.Arena as Arena

type TypeContextReader = ReaderT (TypedWithInferVarsADTArena, TypedWithInferVarsTypeSynonymArena, QuantVarArena)

get_error_type_context :: TypeContextReader StateWithInferVars ErrorTypeContext
get_error_type_context =
    lift get >>= \ infer_vars ->
    ask >>= \ (adts, type_synonyms, vars) ->
    pure (ErrorTypeContext adts type_synonyms vars infer_vars)

data UnifyError
    = Mismatch TypeWithInferVars TypeWithInferVars
    | OccursCheck InferVarKey TypeWithInferVars

data Solve1Result
    = Ok
    | Error Error
    | Defer

type VarSubGenerator = StateT VarSub
type VarSubMap = Map.Map Type.QuantVarKey VarSub
newtype VarSub = VarSub Int deriving Eq
generate_var_sub :: Applicative m => VarSubGenerator m VarSub
generate_var_sub = StateT $ \ cur_var@(VarSub cur_num) -> pure (cur_var, VarSub $ cur_num + 1)
run_var_sub_generator :: Monad m => VarSubGenerator m r -> m r
run_var_sub_generator s = evalStateT s (VarSub 0)

solve :: TypedWithInferVarsADTArena -> TypedWithInferVarsTypeSynonymArena -> QuantVarArena -> [Constraint] -> StateWithInferVars [Constraint]
solve adts type_synonyms vars constraints = runReaderT (solve' constraints) (adts, type_synonyms, vars)
    where
        solve' constraints = do
            next_constraints <- constraints
                -- try to solve each constraint and save each one that couldn't be solved in this round
                & mapM (\ constraint ->
                    solve1 constraint >>= \case
                        Ok -> pure Nothing
                        Error err -> do
                            lift $ lift $ Compiler.tell_error err
                            pure Nothing
                        Defer -> pure (Just constraint)
                )
                & fmap catMaybes

            if length constraints == length next_constraints
                then pure next_constraints -- all constraints were deferred, so no more solving can be done; return these unsolvable constraints
                else solve' next_constraints

-- TODO: figure out how to gracefully handle errors because the infer_varnowns become ambiguous if they cant be unified
solve1 :: Constraint -> TypeContextReader StateWithInferVars Solve1Result
solve1 (Eq in_what sp a b) =
    run_var_sub_generator (runExceptT (unify (unlocate a, Map.empty) (unlocate b, Map.empty))) >>= \case
        Right () -> pure Ok

        Left (Mismatch a_part b_part) ->
            get_error_type_context >>= \ context ->
            pure (Error $ EqError { eq_error_context = context, eq_error_in_what = in_what, eq_error_span = sp, eq_error_a_whole = a, eq_error_b_whole = b, eq_error_a_part = a_part, eq_error_b_part = b_part })

        Left (OccursCheck infer_var ty) ->
            get_error_type_context >>= \ context ->
            pure (Error $ OccursCheckError context sp infer_var ty)

solve1 (Expect in_what got expect) =
    run_var_sub_generator (runExceptT (unify (unlocate got, Map.empty) (expect, Map.empty))) >>= \case
        Right () -> pure Ok

        Left (Mismatch got_part expect_part) ->
            get_error_type_context >>= \ context ->
            pure (Error $ ExpectError { expect_error_context = context, expect_error_in_what = in_what, expect_error_got_whole = got, expect_error_expect_whole = expect, expect_error_got_part = got_part, expect_error_expect_part = expect_part })

        Left (OccursCheck infer_var ty) ->
            get_error_type_context >>= \ context ->
            pure (Error $ OccursCheckError context (just_span got) infer_var ty)

solve1 (UnkIsApplyResult sp infer_var ty arg) =
    apply_ty sp ty arg >>= \case
        Just (Right applied) ->
            run_var_sub_generator (runExceptT (unify_infer_var (infer_var, Map.empty) (applied, Map.empty) False)) >>= \case
                Right () -> pure Ok

                Left (Mismatch infer_var_part applied_part) ->
                    get_error_type_context >>= \ context ->
                    pure (Error $ ExpectError { expect_error_context = context, expect_error_in_what = InTypeApplication, expect_error_got_whole = Located sp applied, expect_error_expect_whole = Type'InferVar infer_var, expect_error_got_part = applied_part, expect_error_expect_part = infer_var_part })

                Left (OccursCheck infer_var ty) ->
                    get_error_type_context >>= \ context ->
                    pure (Error $ OccursCheckError context sp infer_var ty)

        Just (Left e) -> pure (Error e)
        Nothing -> pure Defer

apply_ty :: Span -> TypeWithInferVars -> TypeWithInferVars -> TypeContextReader StateWithInferVars (Maybe (Either Error TypeWithInferVars))
apply_ty sp (Type'InferVar infer_var) arg =
    Arena.get <$> lift get <*> pure infer_var >>= \case
        InferVar _ (Substituted sub) -> apply_ty sp sub arg
        InferVar _ Fresh -> pure Nothing
apply_ty sp ty@(Type'ADT adt params_already_applied) arg = do
    (adts, _, _) <- ask
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
            lift get >>= \ infer_vars ->
            pure (Just $ Right $ substitute infer_vars first_var arg ty)
        more_1:more_more ->
            lift get >>= \ infer_vars ->
            pure (Just $ Right $ Type'Forall (more_1 :| more_more) (substitute infer_vars first_var arg ty))

unify :: (TypeWithInferVars, VarSubMap) -> (TypeWithInferVars, VarSubMap) -> ExceptT UnifyError (VarSubGenerator (TypeContextReader StateWithInferVars)) ()
unify (Type'InferVar a, a_var_map) b = unify_infer_var (a, a_var_map) b False
unify a (Type'InferVar b, b_var_map) = unify_infer_var (b, b_var_map) a True

unify (Type'ADT a_adt_key a_params, a_var_map) (Type'ADT b_adt_key b_params, b_var_map)
    | a_adt_key == b_adt_key
        && length a_params == length b_params =
        mapM_
            (\ (a_param, b_param) -> unify (a_param, a_var_map) (b_param, b_var_map))
            (zip a_params b_params )

unify (Type'Synonym a_syn_key, a_var_map) b =
    lift (lift ask) >>= \ (_, type_synonyms, _) ->
    case Arena.get type_synonyms a_syn_key of
        Type.TypeSynonym _ _ (_, a_expansion) -> unify (a_expansion, a_var_map) b

unify a (Type'Synonym b_syn_key, b_var_map) =
    lift (lift ask) >>= \ (_, type_synonyms, _) ->
    case Arena.get type_synonyms b_syn_key of
        Type.TypeSynonym _ _ (_, b_expansion) -> unify a (b_expansion, b_var_map)

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
            let map1' = Map.insertWith (\ _ _ -> error "variable substitution already in map") var1 new_var_sub map1
                map2' = Map.insertWith (\ _ _ -> error "variable substitution already in map") var2 new_var_sub map2
            in go vars1 t1 map1' vars2 t2 map2'

        go vars1 t1 map1 vars2 t2 map2 = unify (remake_forall_ty vars1 t1, map1) (remake_forall_ty vars2 t2, map2)

        remake_forall_ty [] t1 = t1
        remake_forall_ty (v1:vmore) t1 = Type'Forall (v1 :| vmore) t1
unify (a, _) (b, _) = ExceptT (pure $ Left $ Mismatch a b)

set_infer_var_status :: InferVarKey -> InferVarStatus -> TypeContextReader StateWithInferVars ()
set_infer_var_status infer_var new_status = lift $ modify $ \ ty_arena -> Arena.modify ty_arena infer_var (\ (InferVar for _) -> InferVar for new_status)

unify_infer_var :: (InferVarKey, VarSubMap) -> (TypeWithInferVars, VarSubMap) -> Bool -> ExceptT UnifyError (VarSubGenerator (TypeContextReader StateWithInferVars)) ()
unify_infer_var (infer_var, infer_var_var_map) (other, other_var_map) infer_var_on_right = Arena.get <$> lift (lift $ lift get) <*> pure infer_var >>= \case
    -- if this infer_varnown can be expanded, unify its expansion
    InferVar _ (Substituted infer_var_sub) ->
        if infer_var_on_right
            then unify (other, other_var_map) (infer_var_sub, infer_var_var_map)
            else unify (infer_var_sub, infer_var_var_map) (other, other_var_map)

    -- if this infer_varnown has no substitution, what happens depends on the other type
    InferVar _ Fresh ->
        case other of
            Type'InferVar other_infer_var ->
                Arena.get <$> lift (lift $ lift get) <*> pure other_infer_var >>= \case
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

occurs_check :: InferVarKey -> TypeWithInferVars -> TypeContextReader StateWithInferVars Bool
-- does the infer_varnown u occur anywhere in the type ty?
occurs_check u (Type'InferVar other_v) =
    if u == other_v
        then pure True
        else
            Arena.get <$> lift get <*> pure other_v >>= \case
                InferVar _ (Substituted other_sub) -> occurs_check u other_sub
                InferVar _ Fresh -> pure False

occurs_check u (Type'ADT _ params) = or <$> mapM (occurs_check u) params

occurs_check u (Type'Synonym syn_key) =
    -- TODO: reconsider if this is correct
    ask >>= \ (_, type_synonyms, _) ->
    let Type.TypeSynonym _ _ (_, other_expansion) = Arena.get type_synonyms syn_key
    in occurs_check u other_expansion

occurs_check _ Type'Int = pure False
occurs_check _ Type'Float = pure False
occurs_check _ Type'Char = pure False
occurs_check _ Type'String = pure False
occurs_check _ Type'Bool = pure False
occurs_check u (Type'Function a r) = (||) <$> occurs_check u a <*> occurs_check u r
occurs_check u (Type'Tuple a b) = (||) <$> occurs_check u a <*> occurs_check u b
occurs_check _ (Type'QuantVar _) = pure False
occurs_check u (Type'Forall _ ty) = occurs_check u ty
