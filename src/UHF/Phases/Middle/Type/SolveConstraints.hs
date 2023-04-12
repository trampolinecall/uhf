module UHF.Phases.Middle.Type.SolveConstraints (solve) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type

import qualified UHF.Compiler as Compiler

import UHF.IO.Located (Located (..))
import UHF.IO.Span (Span)

import UHF.Phases.Middle.Type.Unknown
import UHF.Phases.Middle.Type.Aliases
import UHF.Phases.Middle.Type.Error
import UHF.Phases.Middle.Type.Constraint
import UHF.Phases.Middle.Type.StateWithUnk

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

solve :: TypedWithUnkADTArena -> TypedWithUnkTypeSynonymArena -> TypeVarArena -> [Constraint] -> StateWithUnk ()
solve adts type_synonyms vars constraints =
    runReaderT (solve' constraints) (adts, type_synonyms, vars)
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
                then pure () -- deferred all constraints, removing unknowns will report all the ambiguous types
                else solve' next_constraints

-- TODO: figure out how to gracefully handle errors because the type variables become ambiguous if they cant be unified
solve1 :: Constraint -> TypeContextReader StateWithUnk Solve1Result
solve1 (Eq in_what sp a b) =
    runExceptT (unify (unlocate a) (unlocate b)) >>= \case
        Right () -> pure Ok

        Left (Mismatch a_part b_part) ->
            get_error_type_context >>= \ context ->
            pure (Error $ EqError { eq_error_context = context, eq_error_in_what = in_what, eq_error_span = sp, eq_error_a_whole = a, eq_error_b_whole = b, eq_error_a_part = a_part, eq_error_b_part = b_part })

        Left (OccursCheck var ty) ->
            get_error_type_context >>= \ context ->
            pure (Error $ OccursCheckError context sp var ty)

solve1 (Expect in_what got expect) =
    runExceptT (unify (unlocate got) expect) >>= \case
        Right () -> pure Ok

        Left (Mismatch got_part expect_part) ->
            get_error_type_context >>= \ context ->
            pure (Error $ ExpectError { expect_error_context = context, expect_error_in_what = in_what, expect_error_got_whole = got, expect_error_expect_whole = expect, expect_error_got_part = got_part, expect_error_expect_part = expect_part })

        Left (OccursCheck var ty) ->
            get_error_type_context >>= \ context ->
            pure (Error $ OccursCheckError context (just_span got) var ty)

solve1 (UnkIsApplyResult sp unk ty arg) =
    apply_ty sp ty arg >>= \case
        Just (Right applied) ->
            runExceptT (unify_unk unk applied False) >>= \case
                Right () -> pure Ok

                Left (Mismatch unk_part applied_part) ->
                    get_error_type_context >>= \ context ->
                    pure (Error $ ExpectError { expect_error_context = context, expect_error_in_what = InTypeApplication, expect_error_got_whole = Located sp applied, expect_error_expect_whole = Type.Type'Unknown unk, expect_error_got_part = applied_part, expect_error_expect_part = unk_part })

                Left (OccursCheck var ty) ->
                    get_error_type_context >>= \ context ->
                    pure (Error $ OccursCheckError context sp var ty)

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
        [] -> Just <$> (Right <$> subst first_var arg ty)
        more_1:more_more -> Just <$> (Right <$> (Type.Type'Forall (more_1 :| more_more) <$> subst first_var arg ty))
    where
        subst looking_for replacement ty@(Type.Type'Unknown unk) =
            Arena.get <$> lift get <*> pure unk >>= \case
                TypeUnknown _ (Substituted sub) -> subst looking_for replacement sub
                TypeUnknown _ Fresh -> pure ty -- TODO: reconsider if this is correct
        subst looking_for replacement ty@(Type.Type'Variable v)
            | looking_for == v = pure replacement
            | otherwise = pure ty
        subst looking_for replacement (Type.Type'ADT adt_key params) = Type.Type'ADT adt_key <$> mapM (subst looking_for replacement) params
        subst _ _ ty@(Type.Type'Synonym _) = pure ty -- TODO: replace in arguments
        subst _ _ Type.Type'Int = pure Type.Type'Int
        subst _ _ Type.Type'Float = pure Type.Type'Float
        subst _ _ Type.Type'Char = pure Type.Type'Char
        subst _ _ Type.Type'String = pure Type.Type'String
        subst _ _ Type.Type'Bool = pure Type.Type'Bool
        subst looking_for replacement (Type.Type'Function a r) = Type.Type'Function <$> subst looking_for replacement a <*> subst looking_for replacement r
        subst looking_for replacement (Type.Type'Tuple a b) = Type.Type'Tuple <$> subst looking_for replacement a <*> subst looking_for replacement b
        subst looking_for replacement (Type.Type'Forall vars ty) = Type.Type'Forall vars <$> subst looking_for replacement ty

unify :: TypeWithUnk -> TypeWithUnk -> ExceptT UnifyError (TypeContextReader StateWithUnk) ()
unify (Type.Type'Unknown a) b = unify_unk a b False
unify a (Type.Type'Unknown b) = unify_unk b a True

unify (Type.Type'ADT a_adt_key a_params) (Type.Type'ADT b_adt_key b_params)
    | a_adt_key == b_adt_key
        && length a_params == length b_params =
        zipWithM unify a_params b_params >> pure ()

unify (Type.Type'Synonym a_syn_key) b =
    lift ask >>= \ (_, type_synonyms, _) ->
    case Arena.get type_synonyms a_syn_key of
        Type.TypeSynonym _ _ a_expansion -> unify (SIR.type_expr_type_info a_expansion) b

unify a (Type.Type'Synonym b_syn_key) =
    lift ask >>= \ (_, type_synonyms, _) ->
    case Arena.get type_synonyms b_syn_key of
        Type.TypeSynonym _ _ b_expansion -> unify a (SIR.type_expr_type_info b_expansion)

unify Type.Type'Int Type.Type'Int = pure ()
unify Type.Type'Float Type.Type'Float = pure ()
unify Type.Type'Char Type.Type'Char = pure ()
unify Type.Type'String Type.Type'String = pure ()
unify Type.Type'Bool Type.Type'Bool = pure ()
unify (Type.Type'Function a1 r1) (Type.Type'Function a2 r2) = unify a1 a2 >> unify r1 r2
unify (Type.Type'Tuple a1 b1) (Type.Type'Tuple a2 b2) = unify a1 a2 >> unify b1 b2
-- variables do not automatically unify beacuse one type variable can be used in multiple places
unify (Type.Type'Variable v1) (Type.Type'Variable v2) -- TODO: temporary substitutions
    | False = pure () -- TODO: only check temporary substitutions
-- TODO: unifying forall
unify a b = ExceptT (pure $ Left $ Mismatch a b)

set_type_var_state :: TypeUnknownKey -> TypeUnknownState -> TypeContextReader StateWithUnk ()
set_type_var_state var new_state = lift $ modify $ \ ty_arena -> Arena.modify ty_arena var (\ (TypeUnknown for _) -> TypeUnknown for new_state)

unify_unk :: TypeUnknownKey -> TypeWithUnk -> Bool -> ExceptT UnifyError (TypeContextReader StateWithUnk) ()
unify_unk var other var_on_right = Arena.get <$> lift (lift get) <*> pure var >>= \ case
    -- if this variable can be expanded, unify its expansion
    TypeUnknown _ (Substituted var_sub) ->
        if var_on_right
            then unify other var_sub
            else unify var_sub other

    -- if this variable has no substitution, what happens depends on the other type
    TypeUnknown _ Fresh ->
        case other of
            Type.Type'Unknown other_var ->
                Arena.get <$> lift (lift get) <*> pure other_var >>= \case
                    -- if the other type is a substituted type variable, unify this variable with the other's expansion
                    TypeUnknown _ (Substituted other_var_sub) -> unify_unk var other_var_sub var_on_right

                    -- if the other type is a fresh type variable, both of them are fresh type variables and the only thing that can be done is to unify them
                    TypeUnknown _ Fresh ->
                        when (var /= other_var) $
                            lift (set_type_var_state var (Substituted other))

            -- if the other type is a type and not a variable
            _ -> lift (occurs_check var other) >>= \case
                True -> ExceptT (pure $ Left $ OccursCheck var other)
                False -> lift (set_type_var_state var (Substituted other))

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
    ask >>= \ (_, type_synonyms, _) ->
    let Type.TypeSynonym _ _ other_expansion = Arena.get type_synonyms syn_key
    in occurs_check u (SIR.type_expr_type_info other_expansion)

occurs_check _ Type.Type'Int = pure False
occurs_check _ Type.Type'Float = pure False
occurs_check _ Type.Type'Char = pure False
occurs_check _ Type.Type'String = pure False
occurs_check _ Type.Type'Bool = pure False
occurs_check u (Type.Type'Function a r) = (||) <$> occurs_check u a <*> occurs_check u r
occurs_check u (Type.Type'Tuple a b) = (||) <$> occurs_check u a <*> occurs_check u b
occurs_check _ (Type.Type'Variable _) = pure False
occurs_check u (Type.Type'Forall _ ty) = occurs_check u ty
