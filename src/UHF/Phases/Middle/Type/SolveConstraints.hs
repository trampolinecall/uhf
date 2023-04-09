module UHF.Phases.Middle.Type.SolveConstraints (solve) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type

import qualified UHF.Compiler as Compiler

import UHF.IO.Located (Located (..))

import UHF.Phases.Middle.Type.Unknown
import UHF.Phases.Middle.Type.Aliases
import UHF.Phases.Middle.Type.Error
import UHF.Phases.Middle.Type.Constraint
import UHF.Phases.Middle.Type.StateWithUnk

type TypeContextReader = ReaderT (TypedWithUnkADTArena, TypedWithUnkTypeSynonymArena)

data UnifyError
    = Mismatch TypeWithUnk TypeWithUnk
    | OccursCheck TypeUnknownKey TypeWithUnk

data Solve1Result
    = Ok
    | Error Error
    | Defer

read_adts :: Applicative m => TypeContextReader m TypedWithUnkADTArena
read_adts = ReaderT (\ (adts, _) -> pure adts)
read_synonyms :: Applicative m => TypeContextReader m TypedWithUnkTypeSynonymArena
read_synonyms = ReaderT (\ (_, synonyms) -> pure synonyms)

solve :: TypedWithUnkADTArena -> TypedWithUnkTypeSynonymArena -> [Constraint] -> StateWithUnk ()
solve adts type_synonyms constraints =
    runReaderT (solve' constraints) (adts, type_synonyms)
    where
        solve' constraints =
            mapM (\ cons -> (cons,) <$> solve1 cons) constraints >>= \ results ->
            catMaybes <$> mapM
                (\ (cons, res) ->
                    case res of
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
            lift get >>= \ vars ->
            read_adts >>= \ adts ->
            read_synonyms >>= \ type_synonyms ->
            pure (Error $ EqError { eq_error_adts = adts, eq_error_type_synonyms = type_synonyms, eq_error_vars = vars, eq_error_in_what = in_what, eq_error_span = sp, eq_error_a_whole = a, eq_error_b_whole = b, eq_error_a_part = a_part, eq_error_b_part = b_part })

        Left (OccursCheck var ty) ->
            lift get >>= \ vars ->
            read_adts >>= \ adts ->
            read_synonyms >>= \ type_synonyms ->
            pure (Error $ OccursCheckError adts type_synonyms vars sp var ty)

solve1 (Expect in_what got expect) =
    runExceptT (unify (unlocate got) expect) >>= \case
        Right () -> pure Ok

        Left (Mismatch got_part expect_part) ->
            lift get >>= \ vars ->
            read_adts >>= \ adts ->
            read_synonyms >>= \ type_synonyms ->
            pure (Error $ ExpectError { expect_error_adts = adts, expect_error_type_synonyms = type_synonyms, expect_error_vars = vars, expect_error_in_what = in_what, expect_error_got_whole = got, expect_error_expect_whole = expect, expect_error_got_part = got_part, expect_error_expect_part = expect_part })

        Left (OccursCheck var ty) ->
            lift get >>= \ vars ->
            read_adts >>= \ adts ->
            read_synonyms >>= \ type_synonyms ->
            pure (Error $ OccursCheckError adts type_synonyms vars (just_span got) var ty)

solve1 (UnkIsApplyResult sp unk ty arg) =
    apply_ty ty arg >>= \case
        Just (Right applied) ->
            runExceptT (unify_unk unk applied False) >>= \case
                Right () -> pure Ok

                Left (Mismatch unk_part applied_part) ->
                    lift get >>= \ vars ->
                    read_adts >>= \ adts ->
                    read_synonyms >>= \ type_synonyms ->
                    pure (Error $ ExpectError { expect_error_adts = adts, expect_error_type_synonyms = type_synonyms, expect_error_vars = vars, expect_error_in_what = InTypeApplication, expect_error_got_whole = Located sp applied, expect_error_expect_whole = Type.Type'Unknown unk, expect_error_got_part = applied_part, expect_error_expect_part = unk_part })

                Left (OccursCheck var ty) ->
                    lift get >>= \ vars ->
                    read_adts >>= \ adts ->
                    read_synonyms >>= \ type_synonyms ->
                    pure (Error $ OccursCheckError adts type_synonyms vars sp var ty)

        Just (Left ()) -> pure (Error $ todo) -- TODO: error "cannot apply type argument to type"
        Nothing -> pure $ Defer

apply_ty :: TypeWithUnk -> TypeWithUnk -> TypeContextReader StateWithUnk (Maybe (Either () TypeWithUnk))
-- TODO: errors
apply_ty (Type.Type'Unknown unk) arg =
    Arena.get <$> lift get <*> pure unk >>= \case
        TypeUnknown _ (Substituted sub) -> apply_ty sub arg
        TypeUnknown _ Fresh -> pure Nothing
apply_ty (Type.Type'ADT _) _ = pure $ Just $ Left () -- TODO: parameterized datatypes
apply_ty (Type.Type'Synonym _) _ = pure $ Just $ Left () -- TODO: type synonyms with arguments
apply_ty Type.Type'Int _ = pure $ Just $ Left ()
apply_ty Type.Type'Float _ = pure $ Just $ Left ()
apply_ty Type.Type'Char _ = pure $ Just $ Left ()
apply_ty Type.Type'String _ = pure $ Just $ Left ()
apply_ty Type.Type'Bool _ = pure $ Just $ Left ()
apply_ty (Type.Type'Function _ _) _ = pure $ Just $ Left ()
apply_ty (Type.Type'Tuple _ _) _ = pure $ Just $ Left ()
apply_ty (Type.Type'Variable _) _ = pure $ Just $ Left () -- TODO: higher kinded types
apply_ty (Type.Type'Forall (first_var:more_vars) ty) arg
    | null more_vars = Just <$> (Right <$> subst first_var arg ty)
    | otherwise = Just <$> (Right <$> (Type.Type'Forall more_vars <$> (subst first_var arg ty)))
    where
        subst var replacement ty@(Type.Type'Unknown unk) =
            Arena.get <$> lift get <*> pure unk >>= \case
                TypeUnknown _ (Substituted sub) -> subst var replacement sub
                TypeUnknown _ Fresh -> pure ty -- TODO: reconsider if this is correct
        subst var replacement ty@(Type.Type'Variable v)
            | var == v = pure replacement
            | otherwise = pure ty
        subst _ _ ty@(Type.Type'ADT _) = pure ty -- TODO: replace in arguments
        subst _ _ ty@(Type.Type'Synonym _) = pure ty -- TODO: replace in arguments
        subst _ _ Type.Type'Int = pure Type.Type'Int
        subst _ _ Type.Type'Float = pure Type.Type'Float
        subst _ _ Type.Type'Char = pure Type.Type'Char
        subst _ _ Type.Type'String = pure Type.Type'String
        subst _ _ Type.Type'Bool = pure Type.Type'Bool
        subst var replacement (Type.Type'Function a r) = Type.Type'Function <$> (subst var replacement a) <*> (subst var replacement r)
        subst var replacement (Type.Type'Tuple a b) = Type.Type'Tuple <$> (subst var replacement a) <*> (subst var replacement b)
        subst var replacement (Type.Type'Forall vars ty) = (Type.Type'Forall vars <$> (subst var replacement ty))

apply_ty (Type.Type'Forall [] ty) _ = pure $ Just $ Left () -- TODO: replace with NonEmpty

unify :: TypeWithUnk -> TypeWithUnk -> ExceptT UnifyError (TypeContextReader StateWithUnk) ()
unify (Type.Type'Unknown a) b = unify_unk a b False
unify a (Type.Type'Unknown b) = unify_unk b a True

unify (Type.Type'ADT a_adt_key) (Type.Type'ADT b_adt_key)
    | a_adt_key == b_adt_key = pure ()

unify (Type.Type'Synonym a_syn_key) b =
    lift read_synonyms >>= \ type_synonyms ->
    case Arena.get type_synonyms a_syn_key of
        Type.TypeSynonym _ _ a_expansion -> unify (SIR.type_expr_type_info a_expansion) b

unify a (Type.Type'Synonym b_syn_key) =
    lift read_synonyms >>= \ type_synonyms ->
    case Arena.get type_synonyms b_syn_key of
        Type.TypeSynonym _ _ b_expansion -> unify a (SIR.type_expr_type_info b_expansion)

unify Type.Type'Int Type.Type'Int = pure ()
unify Type.Type'Float Type.Type'Float = pure ()
unify Type.Type'Char Type.Type'Char = pure ()
unify Type.Type'String Type.Type'String = pure ()
unify Type.Type'Bool Type.Type'Bool = pure ()
unify (Type.Type'Function a1 r1) (Type.Type'Function a2 r2) = unify a1 a2 >> unify r1 r2
unify (Type.Type'Tuple a1 b1) (Type.Type'Tuple a2 b2) = unify a1 a2 >> unify b1 b2
unify (Type.Type'Variable v1) (Type.Type'Variable v2) -- TODO: temporary substitutions
    | v1 == v2 = pure ()
-- TODO: unifying forall
unify a b = ExceptT (pure $ Left $ Mismatch a b)

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

set_type_var_state :: TypeUnknownKey -> TypeUnknownState -> TypeContextReader StateWithUnk ()
set_type_var_state var new_state = lift $ modify $ \ ty_arena -> Arena.modify ty_arena var (\ (TypeUnknown for _) -> TypeUnknown for new_state)

occurs_check :: TypeUnknownKey -> TypeWithUnk -> TypeContextReader StateWithUnk Bool
-- does the variable v occur anywhere in the type ty?
occurs_check v (Type.Type'Unknown other_v) =
    if v == other_v
        then pure True
        else
            Arena.get <$> lift get <*> pure other_v >>= \case
                TypeUnknown _ (Substituted other_sub) -> occurs_check v other_sub
                TypeUnknown _ Fresh -> pure False

occurs_check _ (Type.Type'ADT _) = pure False

occurs_check v (Type.Type'Synonym syn_key) =
    read_synonyms >>= \ type_synonyms ->
    let Type.TypeSynonym _ _ other_expansion = Arena.get type_synonyms syn_key
    in occurs_check v (SIR.type_expr_type_info other_expansion)

occurs_check _ Type.Type'Int = pure False
occurs_check _ Type.Type'Float = pure False
occurs_check _ Type.Type'Char = pure False
occurs_check _ Type.Type'String = pure False
occurs_check _ Type.Type'Bool = pure False
occurs_check v (Type.Type'Function a r) = (||) <$> occurs_check v a <*> occurs_check v r
occurs_check v (Type.Type'Tuple a b) = (||) <$> occurs_check v a <*> occurs_check v b
occurs_check _ (Type.Type'Variable _) = pure False
occurs_check v (Type.Type'Forall _ ty) = occurs_check v ty
