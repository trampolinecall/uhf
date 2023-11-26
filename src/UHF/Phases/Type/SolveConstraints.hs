module UHF.Phases.Type.SolveConstraints (solve) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.Type as Type

import qualified UHF.Compiler as Compiler

import UHF.IO.Located (Located (..))
import UHF.IO.Span (Span)

import UHF.Phases.Type.Unknown
import UHF.Phases.Type.Aliases
import UHF.Phases.Type.Error
import UHF.Phases.Type.Constraint
import UHF.Phases.Type.StateWithUnk
import UHF.Phases.Type.Utils

import qualified Data.Map as Map

type TypeContextReader = ReaderT (TypedWithUnkADTArena, TypedWithUnkTypeSynonymArena, TypeVarArena)

get_error_type_context :: TypeContextReader StateWithUnk ErrorTypeContext
get_error_type_context =
    lift get >>= \ unks ->
    ask >>= \ (adts, type_synonyms, vars) ->
    pure (ErrorTypeContext adts type_synonyms vars unks)

data UnifyError
    = Mismatch TypeWithUnk TypeWithUnk
    | OccursCheck TypeUnknownKey TypeWithUnk

data Solve1Result
    = Ok
    | Error Error
    | Defer

type VarSubGenerator = StateT VarSub
type VarSubMap = Map.Map Type.TypeVarKey VarSub
newtype VarSub = VarSub Int deriving Eq
generate_var_sub :: Applicative m => VarSubGenerator m VarSub
generate_var_sub = StateT $ \ cur_var@(VarSub cur_num) -> pure (cur_var, VarSub $ cur_num + 1)
run_var_sub_generator :: Monad m => VarSubGenerator m r -> m r
run_var_sub_generator s = evalStateT s (VarSub 0)

solve :: TypedWithUnkADTArena -> TypedWithUnkTypeSynonymArena -> TypeVarArena -> [Constraint] -> StateWithUnk ()
solve adts type_synonyms vars constraints = runReaderT (solve' constraints) (adts, type_synonyms, vars)
    where
        solve' constraints =
            mapM (\ cons -> (cons,) <$> solve1 cons) constraints >>= \ results ->
            catMaybes <$> mapM
                (\ (cons, res) -> case res of
                        Ok -> pure Nothing
                        Error err -> lift (lift $ Compiler.tell_error err) >> pure Nothing
                        Defer -> pure (Just cons)
                )
                results >>= \ next_constraints ->
            if length constraints == length next_constraints
                then pure () -- deferred all constraints, removing unknowns phase will report all the ambiguous types
                else solve' next_constraints

-- TODO: figure out how to gracefully handle errors because the unknowns become ambiguous if they cant be unified
solve1 :: Constraint -> TypeContextReader StateWithUnk Solve1Result
solve1 (Eq in_what sp a b) =
    run_var_sub_generator (runExceptT (unify (unlocate a, Map.empty) (unlocate b, Map.empty))) >>= \case
        Right () -> pure Ok

        Left (Mismatch a_part b_part) ->
            get_error_type_context >>= \ context ->
            pure (Error $ EqError { eq_error_context = context, eq_error_in_what = in_what, eq_error_span = sp, eq_error_a_whole = a, eq_error_b_whole = b, eq_error_a_part = a_part, eq_error_b_part = b_part })

        Left (OccursCheck unk ty) ->
            get_error_type_context >>= \ context ->
            pure (Error $ OccursCheckError context sp unk ty)

solve1 (Expect in_what got expect) =
    run_var_sub_generator (runExceptT (unify (unlocate got, Map.empty) (expect, Map.empty))) >>= \case
        Right () -> pure Ok

        Left (Mismatch got_part expect_part) ->
            get_error_type_context >>= \ context ->
            pure (Error $ ExpectError { expect_error_context = context, expect_error_in_what = in_what, expect_error_got_whole = got, expect_error_expect_whole = expect, expect_error_got_part = got_part, expect_error_expect_part = expect_part })

        Left (OccursCheck unk ty) ->
            get_error_type_context >>= \ context ->
            pure (Error $ OccursCheckError context (just_span got) unk ty)

solve1 (UnkIsApplyResult sp unk ty arg) =
    apply_ty sp ty arg >>= \case
        Just (Right applied) ->
            run_var_sub_generator (runExceptT (unify_unk (unk, Map.empty) (applied, Map.empty) False)) >>= \case
                Right () -> pure Ok

                Left (Mismatch unk_part applied_part) ->
                    get_error_type_context >>= \ context ->
                    pure (Error $ ExpectError { expect_error_context = context, expect_error_in_what = InTypeApplication, expect_error_got_whole = Located sp applied, expect_error_expect_whole = Type.Type'Unknown unk, expect_error_got_part = applied_part, expect_error_expect_part = unk_part })

                Left (OccursCheck unk ty) ->
                    get_error_type_context >>= \ context ->
                    pure (Error $ OccursCheckError context sp unk ty)

        Just (Left e) -> pure (Error e)
        Nothing -> pure Defer

apply_ty :: Span -> TypeWithUnk -> TypeWithUnk -> TypeContextReader StateWithUnk (Maybe (Either Error TypeWithUnk))
apply_ty sp (Type.Type'Unknown unk) arg =
    Arena.get <$> lift get <*> pure unk >>= \case
        TypeUnknown _ (Substituted sub) -> apply_ty sp sub arg
        TypeUnknown _ Fresh -> pure Nothing
apply_ty sp ty@(Type.Type'ADT adt params_already_applied) arg = do
    (adts, _, _) <- ask
    let (Type.ADT _ _ type_params _) = Arena.get adts adt
    if length params_already_applied < length type_params -- TODO: check kind of arg when higher kinds are implemented
          then pure $ Just $ Right $ Type.Type'ADT adt (params_already_applied <> [arg])
          else Just <$> (Left <$> (DoesNotTakeTypeArgument <$> get_error_type_context <*> pure sp <*> pure ty))
apply_ty sp ty@(Type.Type'Synonym _) _ = Just <$> (Left <$> (DoesNotTakeTypeArgument <$> get_error_type_context <*> pure sp <*> pure ty)) -- TODO: type synonyms with arguments, also TODO: make sure that type synonyms are always fully applied
apply_ty sp ty@Type.Type'Int _ = Just <$> (Left <$> (DoesNotTakeTypeArgument <$> get_error_type_context <*> pure sp <*> pure ty))
apply_ty sp ty@Type.Type'Float _ = Just <$> (Left <$> (DoesNotTakeTypeArgument <$> get_error_type_context <*> pure sp <*> pure ty))
apply_ty sp ty@Type.Type'Char _ = Just <$> (Left <$> (DoesNotTakeTypeArgument <$> get_error_type_context <*> pure sp <*> pure ty))
apply_ty sp ty@Type.Type'String _ = Just <$> (Left <$> (DoesNotTakeTypeArgument <$> get_error_type_context <*> pure sp <*> pure ty))
apply_ty sp ty@Type.Type'Bool _ = Just <$> (Left <$> (DoesNotTakeTypeArgument <$> get_error_type_context <*> pure sp <*> pure ty))
apply_ty sp ty@(Type.Type'Function _ _) _ = Just <$> (Left <$> (DoesNotTakeTypeArgument <$> get_error_type_context <*> pure sp <*> pure ty))
apply_ty sp ty@(Type.Type'Tuple _ _) _ = Just <$> (Left <$> (DoesNotTakeTypeArgument <$> get_error_type_context <*> pure sp <*> pure ty))
apply_ty sp ty@(Type.Type'Variable _) _ = Just <$> (Left <$> (DoesNotTakeTypeArgument <$> get_error_type_context <*> pure sp <*> pure ty)) -- TODO: higher kinded variables
apply_ty _ (Type.Type'Forall (first_var :| more_vars) ty) arg =
    -- TODO: check kind of first_var when higher kinded variables are implemented
    case more_vars of
        [] ->
            lift get >>= \ unks ->
            pure (Just $ Right $ substitute unks first_var arg ty)
        more_1:more_more ->
            lift get >>= \ unks ->
            pure (Just $ Right $ Type.Type'Forall (more_1 :| more_more) (substitute unks first_var arg ty))

unify :: (TypeWithUnk, VarSubMap) -> (TypeWithUnk, VarSubMap) -> ExceptT UnifyError (VarSubGenerator (TypeContextReader StateWithUnk)) ()
unify (Type.Type'Unknown a, a_var_map) b = unify_unk (a, a_var_map) b False
unify a (Type.Type'Unknown b, b_var_map) = unify_unk (b, b_var_map) a True

unify (Type.Type'ADT a_adt_key a_params, a_var_map) (Type.Type'ADT b_adt_key b_params, b_var_map)
    | a_adt_key == b_adt_key
        && length a_params == length b_params =
        mapM_
            (\ (a_param, b_param) -> unify (a_param, a_var_map) (b_param, b_var_map))
            (zip a_params b_params )

unify (Type.Type'Synonym a_syn_key, a_var_map) b =
    lift (lift ask) >>= \ (_, type_synonyms, _) ->
    case Arena.get type_synonyms a_syn_key of
        Type.TypeSynonym _ _ (_, a_expansion) -> unify (a_expansion, a_var_map) b

unify a (Type.Type'Synonym b_syn_key, b_var_map) =
    lift (lift ask) >>= \ (_, type_synonyms, _) ->
    case Arena.get type_synonyms b_syn_key of
        Type.TypeSynonym _ _ (_, b_expansion) -> unify a (b_expansion, b_var_map)

unify (Type.Type'Int, _) (Type.Type'Int, _) = pure ()
unify (Type.Type'Float, _) (Type.Type'Float, _) = pure ()
unify (Type.Type'Char, _) (Type.Type'Char, _) = pure ()
unify (Type.Type'String, _) (Type.Type'String, _) = pure ()
unify (Type.Type'Bool, _) (Type.Type'Bool, _) = pure ()
unify (Type.Type'Function a1 r1, var_map_1) (Type.Type'Function a2 r2, var_map_2) = unify (a1, var_map_1) (a2, var_map_2) >> unify (r1, var_map_1) (r2, var_map_2)
unify (Type.Type'Tuple a1 b1, var_map_1) (Type.Type'Tuple a2 b2, var_map_2) = unify (a1, var_map_1) (a2, var_map_2) >> unify (b1, var_map_1) (b2, var_map_2)
-- variables are carefully constructed to be unique for every forall
-- for example if '#(T)' appears twice in a source file then both T's are created twice and have different keys in the type var arena so that each one unifies with itself but not with the other
-- implicitly generated expressions that contain #(...) (for example the constructor functions of adts) also create new variables
unify (a@(Type.Type'Variable v1), var_map_1) (b@(Type.Type'Variable v2), var_map_2)
    | v1 == v2 = pure ()
    | otherwise =
        let var_1 = Map.lookup v1 var_map_1
            var_2 = Map.lookup v2 var_map_2
        in case (var_1, var_2) of
            (Just var_1, Just var_2)
                | var_1 == var_2 -> pure ()

            _ -> ExceptT (pure $ Left $ Mismatch a b)

unify (Type.Type'Forall vars1 t1, var_map_1) (Type.Type'Forall vars2 t2, var_map_2) = go (toList vars1) t1 var_map_1 (toList vars2) t2 var_map_2
    where
        go (var1:vars1) t1 map1 (var2:vars2) t2 map2 =
            lift generate_var_sub >>= \ new_var_sub ->
            -- this will error if the smae type variable appears twice in nested foralls
            -- i.e. something like #(T) T -> #(T) T -> T
            -- because variables are constructed to be unique to each forall, this should never error in practice
            let map1' = Map.insertWith (\ _ _ -> error "variable substitution already in map") var1 new_var_sub map1
                map2' = Map.insertWith (\ _ _ -> error "variable substitution already in map") var2 new_var_sub map2
            in go vars1 t1 map1' vars2 t2 map2'

        go vars1 t1 map1 vars2 t2 map2 = unify (remake_forall_ty vars1 t1, map1) (remake_forall_ty vars2 t2, map2)

        remake_forall_ty [] t1 = t1
        remake_forall_ty (v1:vmore) t1 = Type.Type'Forall (v1 :| vmore) t1
unify (a, _) (b, _) = ExceptT (pure $ Left $ Mismatch a b)

set_type_unk_state :: TypeUnknownKey -> TypeUnknownState -> TypeContextReader StateWithUnk ()
set_type_unk_state unk new_state = lift $ modify $ \ ty_arena -> Arena.modify ty_arena unk (\ (TypeUnknown for _) -> TypeUnknown for new_state)

unify_unk :: (TypeUnknownKey, VarSubMap) -> (TypeWithUnk, VarSubMap) -> Bool -> ExceptT UnifyError (VarSubGenerator (TypeContextReader StateWithUnk)) ()
unify_unk (unk, unk_var_map) (other, other_var_map) unk_on_right = Arena.get <$> lift (lift $ lift get) <*> pure unk >>= \case
    -- if this unknown can be expanded, unify its expansion
    TypeUnknown _ (Substituted unk_sub) ->
        if unk_on_right
            then unify (other, other_var_map) (unk_sub, unk_var_map)
            else unify (unk_sub, unk_var_map) (other, other_var_map)

    -- if this unknown has no substitution, what happens depends on the other type
    TypeUnknown _ Fresh ->
        case other of
            Type.Type'Unknown other_unk ->
                Arena.get <$> lift (lift $ lift get) <*> pure other_unk >>= \case
                    -- if the other type is a substituted unknown, unify this unknown with the other's expansion
                    TypeUnknown _ (Substituted other_unk_sub) -> unify_unk (unk, unk_var_map) (other_unk_sub, other_var_map) unk_on_right

                    -- if the other type is a fresh unknown, both of them are fresh unknowns and the only thing that can be done is to unify them
                    TypeUnknown _ Fresh ->
                        when (unk /= other_unk) $
                            lift (lift $ set_type_unk_state unk (Substituted other))

            -- if the other type is a type and not an unknown
            _ -> lift (lift $ occurs_check unk other) >>= \case
                True -> ExceptT (pure $ Left $ OccursCheck unk other)
                False -> lift (lift $ set_type_unk_state unk (Substituted other))

occurs_check :: TypeUnknownKey -> TypeWithUnk -> TypeContextReader StateWithUnk Bool
-- does the unknown u occur anywhere in the type ty?
occurs_check u (Type.Type'Unknown other_v) =
    if u == other_v
        then pure True
        else
            Arena.get <$> lift get <*> pure other_v >>= \case
                TypeUnknown _ (Substituted other_sub) -> occurs_check u other_sub
                TypeUnknown _ Fresh -> pure False

occurs_check u (Type.Type'ADT _ params) = or <$> mapM (occurs_check u) params

occurs_check u (Type.Type'Synonym syn_key) =
    -- TODO: reconsider if this is correct
    ask >>= \ (_, type_synonyms, _) ->
    let Type.TypeSynonym _ _ (_, other_expansion) = Arena.get type_synonyms syn_key
    in occurs_check u other_expansion

occurs_check _ Type.Type'Int = pure False
occurs_check _ Type.Type'Float = pure False
occurs_check _ Type.Type'Char = pure False
occurs_check _ Type.Type'String = pure False
occurs_check _ Type.Type'Bool = pure False
occurs_check u (Type.Type'Function a r) = (||) <$> occurs_check u a <*> occurs_check u r
occurs_check u (Type.Type'Tuple a b) = (||) <$> occurs_check u a <*> occurs_check u b
occurs_check _ (Type.Type'Variable _) = pure False
occurs_check u (Type.Type'Forall _ ty) = occurs_check u ty
