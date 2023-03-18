module UHF.Phases.Middle.Type.SolveConstraints (solve) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.Type as Type

import qualified UHF.Compiler as Compiler

import UHF.IO.Located (Located (..))

import UHF.Phases.Middle.Type.Var
import UHF.Phases.Middle.Type.Aliases
import UHF.Phases.Middle.Type.Error
import UHF.Phases.Middle.Type.Constraint
import UHF.Phases.Middle.Type.StateWithVars

type TypeContextReader = ReaderT (TypedWithVarsADTArena, TypedWithVarsTypeSynonymArena)

read_adts :: Applicative m => TypeContextReader m TypedWithVarsADTArena
read_adts = ReaderT (\ (adts, _) -> pure adts)
read_synonyms :: Applicative m => TypeContextReader m TypedWithVarsTypeSynonymArena
read_synonyms = ReaderT (\ (_, synonyms) -> pure synonyms)

solve :: TypedWithVarsADTArena -> TypedWithVarsTypeSynonymArena -> [Constraint] -> StateWithVars ()
solve adts type_synonyms constraints = runReaderT (mapM_ solve' constraints) (adts, type_synonyms)

-- TODO: figure out how to gracefully handle errors because the type variables become ambiguous if they cant be unified
solve' :: Constraint -> TypeContextReader StateWithVars ()
solve' (Eq in_what sp a b) =
    runExceptT (unify (unlocate a) (unlocate b)) >>= \case
        Right () -> pure ()

        Left (Left (a_part, b_part)) -> -- mismatch error
            lift get >>= \ vars ->
            read_adts >>= \ adts ->
            read_synonyms >>= \ type_synonyms ->
            lift (lift $ Compiler.tell_error $ EqError { eq_error_adts = adts, eq_error_type_synonyms = type_synonyms, eq_error_vars = vars, eq_error_in_what = in_what, eq_error_span = sp, eq_error_a_whole = a, eq_error_b_whole = b, eq_error_a_part = a_part, eq_error_b_part = b_part }) >>
            pure ()

        Left (Right (var, ty)) -> -- occurs check failure
            lift get >>= \ vars ->
            read_adts >>= \ adts ->
            read_synonyms >>= \ type_synonyms ->
            lift (lift $ Compiler.tell_error $ OccursCheckError adts type_synonyms vars sp var ty) >>
            pure ()

solve' (Expect in_what got expect) =
    runExceptT (unify (unlocate got) expect) >>= \case
        Right () -> pure ()

        Left (Left (got_part, expect_part)) -> -- mismatch error
            lift get >>= \ vars ->
            read_adts >>= \ adts ->
            read_synonyms >>= \ type_synonyms ->
            lift (lift $ Compiler.tell_error $ ExpectError { expect_error_adts = adts, expect_error_type_synonyms = type_synonyms, expect_error_vars = vars, expect_error_in_what = in_what, expect_error_got_whole = got, expect_error_expect_whole = expect, expect_error_got_part = got_part, expect_error_expect_part = expect_part }) >>
            pure ()

        Left (Right (var, ty)) -> -- occurs check failure
            lift get >>= \ vars ->
            read_adts >>= \ adts ->
            read_synonyms >>= \ type_synonyms ->
            lift (lift $ Compiler.tell_error $ OccursCheckError adts type_synonyms vars (just_span got) var ty) >>
            pure ()

unify :: TypeWithVars -> TypeWithVars -> ExceptT (Either (TypeWithVars, TypeWithVars) (TypeVarKey, TypeWithVars)) (TypeContextReader StateWithVars) ()
unify a@(Type.Type'ADT a_adt_key) b@(Type.Type'ADT b_adt_key)
    | a_adt_key == b_adt_key = pure ()
    | otherwise = ExceptT (pure $ Left $ Left (a, b))

unify (Type.Type'Synonym a_syn_key) b =
    lift read_synonyms >>= \ type_synonyms ->
    case Arena.get type_synonyms a_syn_key of
        Type.TypeSynonym _ _ a_expansion -> unify a_expansion b

unify a (Type.Type'Synonym b_syn_key) =
    lift read_synonyms >>= \ type_synonyms ->
    case Arena.get type_synonyms b_syn_key of
        Type.TypeSynonym _ _ b_expansion -> unify a b_expansion

unify (Type.Type'Variable a) b = unify_var a b False
unify a (Type.Type'Variable b) = unify_var b a True
unify Type.Type'Int Type.Type'Int = pure ()
unify Type.Type'Float Type.Type'Float = pure ()
unify Type.Type'Char Type.Type'Char = pure ()
unify Type.Type'String Type.Type'String = pure ()
unify Type.Type'Bool Type.Type'Bool = pure ()
unify (Type.Type'Function a1 r1) (Type.Type'Function a2 r2) = unify a1 a2 >> unify r1 r2
unify (Type.Type'Tuple a1 b1) (Type.Type'Tuple a2 b2) = unify a1 a2 >> unify b1 b2
unify a b = ExceptT (pure $ Left $ Left (a, b))

unify_var :: TypeVarKey -> TypeWithVars -> Bool -> ExceptT (Either (TypeWithVars, TypeWithVars) (TypeVarKey, TypeWithVars)) (TypeContextReader StateWithVars) ()
unify_var var other var_on_right = Arena.get <$> lift (lift get) <*> pure var >>= \ case
    -- if this variable can be expanded, unify its expansion
    TypeVar _ (Substituted var_sub) ->
        if var_on_right
            then unify other var_sub
            else unify var_sub other

    -- if this variable has no substitution, what happens depends on the other type
    TypeVar _ Fresh ->
        case other of
            Type.Type'Variable other_var ->
                Arena.get <$> lift (lift get) <*> pure other_var >>= \case
                    -- if the other type is a substituted type variable, unify this variable with the other's expansion
                    TypeVar _ (Substituted other_var_sub) -> unify_var var other_var_sub var_on_right

                    -- if the other type is a fresh type variable, both of them are fresh type variables and the only thing that can be done is to unify them
                    TypeVar _ Fresh ->
                        if var /= other_var
                            then lift (set_type_var_state var (Substituted other))
                            else pure ()

            -- if the other type is a type and not a variable
            _ -> lift (occurs_check var other) >>= \case
                True -> ExceptT (pure $ Left $ Right (var, other))
                False -> lift (set_type_var_state var (Substituted other))

set_type_var_state :: TypeVarKey -> TypeVarState -> TypeContextReader StateWithVars ()
set_type_var_state var new_state = lift $ modify $ \ ty_arena -> Arena.modify ty_arena var (\ (TypeVar for _) -> TypeVar for new_state)

occurs_check :: TypeVarKey -> TypeWithVars -> TypeContextReader StateWithVars Bool
-- does the variable v occur anywhere in the type ty?
occurs_check v (Type.Type'Variable other_v) =
    if v == other_v
        then pure True
        else
            Arena.get <$> lift get <*> pure other_v >>= \case
                TypeVar _ (Substituted other_sub) -> occurs_check v other_sub
                TypeVar _ Fresh -> pure False

occurs_check _ (Type.Type'ADT _) = pure False

occurs_check v (Type.Type'Synonym syn_key) =
    read_synonyms >>= \ type_synonyms ->
    let Type.TypeSynonym _ _ other_expansion = Arena.get type_synonyms syn_key
    in occurs_check v other_expansion

occurs_check _ Type.Type'Int = pure False
occurs_check _ Type.Type'Float = pure False
occurs_check _ Type.Type'Char = pure False
occurs_check _ Type.Type'String = pure False
occurs_check _ Type.Type'Bool = pure False
occurs_check v (Type.Type'Function a r) = (||) <$> occurs_check v a <*> occurs_check v r
occurs_check v (Type.Type'Tuple a b) = (||) <$> occurs_check v a <*> occurs_check v b

