module UHF.Parts.TypeSolver.TypeWithInferVar.PP
    ( InferVarNamer
    , run_infer_var_namer
    , make_infer_var_name_messages
    , pp_type
    )
    where

import UHF.Prelude

import qualified Data.Map as Map

import UHF.Parts.TypeSolver.TypeWithInferVar
import UHF.Source.Located (Located (..))
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.PP as Type.PP
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.PP as PP
import qualified UHF.Util.Arena as Arena

-- this will be fixed in the infer-var-namer branch
-- TODO: figure out a way to have this split into a separate file without creating a import loop
type InferVarNamer = State (Int, Map InferVarKey Text)
run_infer_var_namer :: InferVarNamer a -> (a, Map InferVarKey Text)
run_infer_var_namer s = let (r, (_, n)) = runState s (1, Map.empty) in (r, n)
make_infer_var_name_messages :: InferVarArena -> Map InferVarKey Text -> [Diagnostic.Message]
make_infer_var_name_messages infer_vars names =
    map
        (\ (key, name) ->
            let (InferVar infer_var_for_what _) = Arena.get infer_vars key
                var_sp = infer_var_for_what_sp infer_var_for_what
                var_name = infer_var_for_what_name infer_var_for_what
             in var_sp `Diagnostic.msg_note_at` convert_str ("where '" <> name <> "' is the type of this " <> var_name))
        (Map.toList names)
name_infer_var :: InferVarKey -> InferVarNamer Text
name_infer_var var = state $
    \ (cur_n, cache) ->
        case Map.lookup var cache of
            Just res -> (res, (cur_n, cache))
            Nothing -> let name = "T" <> show cur_n in (name, (cur_n + 1, Map.insert var name cache))
                {- TODO: make this work correctly
                -- print like a base-26 number system
                -- goes 'A', 'B', 'C', ..., 'Y', 'Z', 'AA', 'AB', 'AC', ..., 'AY', 'AZ', 'BA', 'BB', 'BC', ...
                let name = Text.pack $ map to_characters $ reverse $ make_name [] cur_n
                in (name, (cur_n + 1, Map.insert var name cache))
    where
        make_name acc n
            | n > 0 =
                make_name ((n `mod` 26) : acc) (n `div` 26)
            | otherwise = acc

        to_characters n = chr $ 65 + n
    -}

pp_type :: Bool -> Arena.Arena (Type.ADT a) Type.ADTKey -> Arena.Arena (Type.TypeSynonym b) Type.TypeSynonymKey -> Arena.Arena Type.QuantVar Type.QuantVarKey -> InferVarArena -> Type -> InferVarNamer PP.Token -- TODO: put the arenas and things into a reader monad inside InferVarNamer?
pp_type name_infer_vars adts type_synonyms vars infer_vars = go
    where
        -- TODO: it is a bit inelegant to duplicate this with IR.Type.Type because then the pretty printing code (and a lot of the other code) is duplicated and you have remember to change it in both places
        go (Type'ADT k params) =
            mapM go params >>= \ params ->
            let params'
                    | null params = ""
                    | otherwise = PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent params]
            in pure $ PP.List [Type.PP.refer_adt (Arena.get adts k), params']
        go (Type'Synonym k) = pure $ Type.PP.refer_type_synonym $ Arena.get type_synonyms k
        go Type'Int = pure "int"
        go Type'Float = pure "float"
        go Type'Char = pure "char"
        go Type'String = pure "string"
        go Type'Bool = pure "bool"
        go (Type'Function a r) = do
            a_shown <- go a
            r_shown <- go r
            pure (PP.List [a_shown, " -> ", r_shown])
        go (Type'Tuple a b) = do
            a_shown <- go a
            b_shown <- go b
            pure (PP.parenthesized_comma_list PP.Inconsistent [a_shown, b_shown])
        go (Type'InferVar infer_var) =
            case Arena.get infer_vars infer_var of
                InferVar _ Fresh
                    | name_infer_vars -> PP.String <$> name_infer_var infer_var
                    | otherwise -> pure "_"
                InferVar _ (Substituted other) -> go other
        go (Type'QuantVar var) =
            let (Type.QuantVar (Located _ name)) = Arena.get vars var
            in pure $ PP.String name -- TODO: write id
        go (Type'Forall new_vars ty) = do
            ty <- go ty
            pure $ PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent (map (\ vk -> let (Type.QuantVar (Located _ name)) = Arena.get vars vk in PP.String name) (toList new_vars)), " ", ty]
        -- TODO: do kinds correctly
        go Type'Kind'Type = pure $ PP.String "*" -- TODO: this does not seem right
        go (Type'Kind'Arrow a b) = do
            a <- go a
            b <- go b
            pure (PP.List [a, PP.String " -># ", b]) -- TODO: precedence
        go Type'Kind'Kind = pure $ PP.String "<kind>" -- TODO: this is most definitely not correct
