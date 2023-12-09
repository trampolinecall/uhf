module UHF.Phases.NameResolve.Utils
    ( Error (..)
    , WithErrors

    , NameMaps (..)
    , NameMapStack (..)
    , ChildMaps

    , DeclAt (..)
    , DeclChildrenList
    , VariableList
    , ADTVariantList
    , make_name_maps
    , make_name_maps_from_decls
    , collect_child_maps
    , name_maps_to_child_maps
    , child_maps_to_name_maps

    , SIRChildMaps
    , get_module_child_maps

    , get_decl_child
    , get_value_child
    , get_variant_child

    , pattern_vars
    , binding_children

    , NRReader

    , ask_adt_arena -- TODO: remove
    , ask_var_arena -- TODO: remove?
    , ask_type_var_arena -- TODO: remove?
    , ask_sir_child_maps -- TODO: remove?
    ) where

-- TODO: clean up this and the 3 modules too

import UHF.Prelude

import qualified Data.List as List
import qualified Data.Map as Map

import UHF.Source.Located (Located (Located, unlocate))
import UHF.Source.Span (Span)
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Parts.TypeSolver.TypeWithInferVar as TypeWithInferVar
import qualified UHF.Util.Arena as Arena

-- errors {{{1
type WithErrors = Compiler.WithDiagnostics Error Void
data Error
    = Error'CouldNotFind (Located Text)
    | Error'CouldNotFindIn (Maybe (Located Text)) (Located Text)
    | Error'MultipleDecls Text [DeclAt]
    | Error'NotAType Span Text

instance Diagnostic.ToError Error where
    to_error (Error'CouldNotFind (Located sp name)) = Diagnostic.Error (Just sp) ("could not find name '" <> name <> "'") [] []
    to_error (Error'CouldNotFindIn prev (Located sp name)) =
        let message =
                "could not find name '" <> name <> "'"
                    <> case prev of
                        Just (Located _ prev_name) -> "in '" <> prev_name <> "'"
                        Nothing -> ""
        in Diagnostic.Error (Just sp) message [] []

    to_error (Error'MultipleDecls name decl_ats) =
        let span = headMay $ mapMaybe decl_at_span decl_ats -- take the first span of the decl_ats; if there are no decl_ats with a span, then this will be Ntohing
        in Diagnostic.Error
            span
            (show (length decl_ats) <> " declarations of '" <> convert_str name <> "'")
            (map (\ at -> (decl_at_span at, Diagnostic.MsgError, decl_at_message name at)) decl_ats)
            []
        where
            decl_at_span (DeclAt sp) = Just sp
            decl_at_span ImplicitPrim = Nothing
            decl_at_message _ (DeclAt _) = Nothing
            decl_at_message n ImplicitPrim = Just $ "'" <> convert_str n <> "' is implicitly declared as a primitive" -- TODO: reword this message (ideally when it is declared through the prelude import the message would be something like 'implicitly declared by prelude')

    to_error (Error'NotAType sp instead) =
        Diagnostic.Error (Just sp) ("not a type: got " <> instead) [] []

-- child/name maps {{{1
-- these are very similar datatypes; the difference between them is conceptual: ChildMaps is a map that tells what the children of a certain entity are, whereas a NameMap just stores what names are currently in scope (and is only used for resolving roots)
-- this is also why there is a NameMapStack but not a ChildMapStack
data ChildMaps = ChildMaps (Map.Map Text (SIR.Decl TypeWithInferVar.Type)) (Map.Map Text SIR.VariableKey) (Map.Map Text Type.ADT.VariantIndex) deriving Show
data NameMaps = NameMaps (Map.Map Text (SIR.Decl TypeWithInferVar.Type)) (Map.Map Text SIR.VariableKey) (Map.Map Text Type.ADT.VariantIndex) deriving Show
data NameMapStack = NameMapStack NameMaps (Maybe NameMapStack)

-- TODO: do not export this
data DeclAt = DeclAt Span | ImplicitPrim deriving Show

-- TODO: do not export these
type DeclChildrenList = [(Text, DeclAt, SIR.Decl TypeWithInferVar.Type)]
type VariableList = [(Text, DeclAt, SIR.VariableKey)]
type ADTVariantList = [(Text, DeclAt, Type.ADT.VariantIndex)]

-- SIRChildMaps {{{2
data SIRChildMaps = SIRChildMaps (Arena.Arena ChildMaps SIR.ModuleKey)

get_module_child_maps :: SIRChildMaps -> SIR.ModuleKey -> ChildMaps
get_module_child_maps (SIRChildMaps module_child_maps) mk = Arena.get module_child_maps mk
-- making child maps {{{2
-- TODO: do not export this
make_name_maps :: DeclChildrenList -> VariableList -> ADTVariantList -> WithErrors NameMaps
make_name_maps decls variables adt_variants =
    let decl_dups = find_dups decls
        bn_dups = find_dups variables
        variant_dups = find_dups adt_variants
    in report_dups decl_dups >> report_dups bn_dups >> report_dups variant_dups >>
    pure (NameMaps (make_map decls) (make_map variables) (make_map adt_variants))
    where
        -- separate finding duplicates from making maps so that if there is a duplicate the whole name contexet doesnt just disappear
        -- duplicates will just take the last bound name in the last, because of the how Map.fromList is implemented
        find_dups x =
            let grouped = List.groupBy ((==) `on` get_name) $ List.sortBy (compare `on` get_name) x -- compare names of bound names only
            in filter ((1/=) . length) grouped
            where
                get_name (n, _, _) = n

        make_map x = Map.fromList (map (\ (n, _, v) -> (n, v)) x)

        report_dups = mapM_
            (\ decls ->
                let (first_name, _, _) = head decls
                in Compiler.tell_error $ Error'MultipleDecls first_name (map get_decl_at decls))
            where
                get_decl_at (_, d, _) = d

-- TODO: put this in NRReader monad?
make_name_maps_from_decls :: Arena.Arena (Type.TypeSynonym ty) Type.TypeSynonymKey -> [SIR.Binding stage] -> [Type.ADTKey] -> [Type.TypeSynonymKey] -> NRReader (Arena.Arena (Type.ADT (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage)) Type.ADTKey) (Arena.Arena (SIR.Variable stage) SIR.VariableKey) type_var_arena sir_child_maps WithErrors NameMaps
make_name_maps_from_decls type_synonym_arena bindings adts type_synonyms =
    unzip3 <$> mapM binding_children bindings >>= \ (binding_decl_entries, binding_var_entries, binding_variant_entries) ->
    ask_adt_arena >>= \ adt_arena ->
    let (adt_decl_entries, adt_var_entries, adt_variant_entries) =
            adts
                & map
                    (\ adt ->
                        let (Type.ADT _ (Located name_sp name) _ _) = Arena.get adt_arena adt
                            adt_decl_key = SIR.Decl'Type $ TypeWithInferVar.Type'ADT adt []
                        in ([(name, DeclAt name_sp, adt_decl_key)], [], []) -- constructor vars and variants handled by adt variant bindings, TODO: make it not the case so that this can deal with named variants too
                    )
                & unzip3
        (type_synonym_decl_entries, type_synonym_var_entries, type_synonym_variant_entries) =
            type_synonyms
                & map
                    (\ synonym ->
                        let (Type.TypeSynonym _ (Located name_sp name) _) = Arena.get type_synonym_arena synonym
                            synonym_decl_key = SIR.Decl'Type $ TypeWithInferVar.Type'Synonym synonym
                        in ([(name, DeclAt name_sp, synonym_decl_key)], [], [])
                    )
                & unzip3
    in lift $ make_name_maps
            (concat $ binding_decl_entries ++ adt_decl_entries ++ type_synonym_decl_entries)
            (concat $ binding_var_entries ++ adt_var_entries ++ type_synonym_var_entries)
            (concat $ binding_variant_entries ++ adt_variant_entries ++ type_synonym_variant_entries)

collect_child_maps :: SIR.SIR stage -> WithErrors SIRChildMaps
collect_child_maps (SIR.SIR mod_arena adt_arena type_synonym_arena _ variable_arena _) = SIRChildMaps <$> Arena.transformM go mod_arena
    where
        primitive_decls =
                [ ("int", ImplicitPrim, SIR.Decl'Type TypeWithInferVar.Type'Int)
                , ("float", ImplicitPrim, SIR.Decl'Type TypeWithInferVar.Type'Float)
                , ("char", ImplicitPrim, SIR.Decl'Type TypeWithInferVar.Type'Char)
                , ("string", ImplicitPrim, SIR.Decl'Type TypeWithInferVar.Type'String)
                , ("bool", ImplicitPrim, SIR.Decl'Type TypeWithInferVar.Type'Bool)
                ]
        primitive_vars = []

        go (SIR.Module _ bindings adts type_synonyms) =
            -- TODO: remove runReaderT
            -- TODO: use make_name_maps_from_decls but somehow add primitives
            runReaderT (unzip3 <$> mapM binding_children bindings) (adt_arena, variable_arena, (), ()) >>= \ (binding_decl_entries, binding_var_entries, binding_variant_entries) ->
            let (adt_decl_entries, adt_var_entries, adt_variant_entries) =
                    adts
                        & map
                            (\ adt ->
                                let (Type.ADT _ (Located name_sp name) _ _) = Arena.get adt_arena adt
                                    adt_decl_key = SIR.Decl'Type $ TypeWithInferVar.Type'ADT adt []
                                in ([(name, DeclAt name_sp, adt_decl_key)], [], [])) -- constructor vars and variants handled by adt variant bindings, TODO: make it not the case so that this can deal with named variants too
                        & unzip3

                (type_synonym_decl_entries, type_synonym_var_entries, type_synonym_variant_entries) =
                    type_synonyms
                        & map
                            (\ synonym ->
                                let (Type.TypeSynonym _ (Located name_sp name) _) = Arena.get type_synonym_arena synonym
                                    synonym_decl_key = SIR.Decl'Type $ TypeWithInferVar.Type'Synonym synonym
                                in ([(name, DeclAt name_sp, synonym_decl_key)], [], []))
                        & unzip3
            in name_maps_to_child_maps
                <$> (make_name_maps
                        (concat $ primitive_decls : binding_decl_entries ++ adt_decl_entries ++ type_synonym_decl_entries)
                        (concat $ primitive_vars : binding_var_entries ++ adt_var_entries ++ type_synonym_var_entries)
                        (concat $ binding_variant_entries ++ adt_variant_entries ++ type_synonym_variant_entries))

name_maps_to_child_maps :: NameMaps -> ChildMaps
name_maps_to_child_maps (NameMaps decl val adtv) = ChildMaps decl val adtv
child_maps_to_name_maps :: ChildMaps -> NameMaps
child_maps_to_name_maps (ChildMaps decl val adtv) = NameMaps decl val adtv

binding_children :: Monad under => SIR.Binding stage -> NRReader (Arena.Arena (Type.ADT (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage)) Type.ADTKey) (Arena.Arena (SIR.Variable stage) SIR.VariableKey) type_var_arena sir_child_maps under (DeclChildrenList, VariableList, ADTVariantList)
binding_children (SIR.Binding pat _ _) = ([],, []) <$> pattern_vars pat
binding_children (SIR.Binding'ADTVariant sp var_key _ variant_index) = var_name var_key >>= \ name -> pure ([], [(name, DeclAt sp, var_key)], [(name, DeclAt sp, variant_index)]) -- TODO: move variants to inside their types, also dont handle adt variants here

pattern_vars :: Monad under => SIR.Pattern stage -> NRReader (Arena.Arena (Type.ADT (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage)) Type.ADTKey) (Arena.Arena (SIR.Variable stage) SIR.VariableKey) type_var_arena sir_child_maps under VariableList
pattern_vars (SIR.Pattern'Identifier _ sp var_key) = var_name var_key >>= \ name -> pure [(name, DeclAt sp, var_key)]
pattern_vars (SIR.Pattern'Wildcard _ _) = pure []
pattern_vars (SIR.Pattern'Tuple _ _ a b) = pattern_vars a >>= \ a -> pattern_vars b >>= \ b -> pure (a ++ b)
pattern_vars (SIR.Pattern'Named _ _ _ (Located var_span var_key) subpat) = var_name var_key >>= \ name -> pattern_vars subpat >>= \ subpat -> pure ((name, DeclAt var_span, var_key) : subpat)
pattern_vars (SIR.Pattern'AnonADTVariant _ _ _ _ _ fields) = concat <$> mapM pattern_vars fields
pattern_vars (SIR.Pattern'NamedADTVariant _ _ _ _ _ fields) = concat <$> mapM (pattern_vars . snd) fields
pattern_vars (SIR.Pattern'Poison _ _) = pure []

var_name :: Monad under => SIR.VariableKey -> NRReader (Arena.Arena (Type.ADT (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage)) Type.ADTKey) (Arena.Arena (SIR.Variable stage) SIR.VariableKey) type_var_arena sir_child_maps under Text
var_name var_key =
    ask_var_arena >>= \ var_arena ->
    case Arena.get var_arena var_key of
        SIR.Variable _ _ (Located _ name) -> pure name
        SIR.Variable'ADTVariant _ variant_index _ _ _ ->
            ask_adt_arena >>= \ adt_arena ->
            let variant = Type.ADT.get_variant adt_arena variant_index
            in pure $ unlocate $ Type.ADT.variant_name variant

-- getting from child maps {{{2
-- TODO: remove duplication from these
get_decl_child :: SIRChildMaps -> SIR.Decl TypeWithInferVar.Type -> Located Text -> Either Error (SIR.Decl TypeWithInferVar.Type)
get_decl_child sir_child_maps decl name =
    let res = case decl of
            SIR.Decl'Module m ->
                let ChildMaps d_children _ _ = get_module_child_maps sir_child_maps m
                in Map.lookup (unlocate name) d_children

            SIR.Decl'Type _ -> Nothing
    in case res of
        Just res -> Right res
        Nothing -> Left $ Error'CouldNotFindIn Nothing name -- TODO: put previous

get_value_child :: SIRChildMaps -> SIR.Decl TypeWithInferVar.Type -> Located Text -> Either Error SIR.VariableKey
get_value_child sir_child_maps decl name =
    let res = case decl of
            SIR.Decl'Module m ->
                let ChildMaps _ v_children _ = get_module_child_maps sir_child_maps m
                in Map.lookup (unlocate name) v_children

            SIR.Decl'Type _ -> Nothing
    in case res of
        Just res -> Right res
        Nothing -> Left $ Error'CouldNotFindIn Nothing name -- TODO: put previous

get_variant_child :: SIRChildMaps -> SIR.Decl TypeWithInferVar.Type -> Located Text -> Either Error Type.ADT.VariantIndex
get_variant_child sir_child_maps decl name =
    let res = case decl of
            SIR.Decl'Module m ->
                let ChildMaps _ _ adtv_children = get_module_child_maps sir_child_maps m
                in Map.lookup (unlocate name) adtv_children

            SIR.Decl'Type _ -> Nothing
    in case res of
        Just res -> Right res
        Nothing -> Left $ Error'CouldNotFindIn Nothing name -- TODO: put previous
-- NRReader {{{1
type NRReader adt_arena var_arena type_var_arena sir_child_maps = ReaderT (adt_arena, var_arena, type_var_arena, sir_child_maps)

ask_adt_arena :: Applicative under => NRReader adt_arena var_arena type_var_arena sir_child_maps under adt_arena
ask_adt_arena = ReaderT $ \ (adts, _, _, _) -> pure adts
ask_var_arena :: Applicative under => NRReader adt_arena var_arena type_var_arena sir_child_maps under var_arena
ask_var_arena = ReaderT $ \ (_, vars, _, _) -> pure vars
ask_type_var_arena :: Applicative under => NRReader adt_arena var_arena type_var_arena sir_child_maps under type_var_arena
ask_type_var_arena = ReaderT $ \ (_, _, tvars, _) -> pure tvars
ask_sir_child_maps :: Applicative under => NRReader adt_arena var_arena type_var_arena sir_child_maps under sir_child_maps
ask_sir_child_maps = ReaderT $ \ (_, _, _, sir_child_maps) -> pure sir_child_maps
