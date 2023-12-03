module UHF.Phases.NameResolve.Utils
    ( Error (..)
    , WithErrors

    , NameMaps (..)
    , NameMapStack (..)
    , ChildMaps

    , DeclAt (..)
    , DeclChildrenList
    , BoundValueList
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

    , pattern_bvs
    , binding_children

    , NRReader

    , ask_adt_arena -- TODO: remove
    , ask_bv_arena -- TODO: remove?
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
import qualified UHF.Data.SIR as SIR
import qualified UHF.Diagnostic as Diagnostic
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
data ChildMaps = ChildMaps (Map.Map Text SIR.Decl) (Map.Map Text SIR.BoundValueKey) (Map.Map Text Type.ADTVariantIndex) deriving Show
data NameMaps = NameMaps (Map.Map Text SIR.Decl) (Map.Map Text SIR.BoundValueKey) (Map.Map Text Type.ADTVariantIndex) deriving Show
data NameMapStack = NameMapStack NameMaps (Maybe NameMapStack)

-- TODO: do not export this
data DeclAt = DeclAt Span | ImplicitPrim deriving Show

-- TODO: do not export these
type DeclChildrenList = [(Text, DeclAt, SIR.Decl)]
type BoundValueList = [(Text, DeclAt, SIR.BoundValueKey)]
type ADTVariantList = [(Text, DeclAt, Type.ADTVariantIndex)]

-- SIRChildMaps {{{2
data SIRChildMaps = SIRChildMaps (Arena.Arena ChildMaps SIR.ModuleKey)

get_module_child_maps :: SIRChildMaps -> SIR.ModuleKey -> ChildMaps
get_module_child_maps (SIRChildMaps module_child_maps) mk = Arena.get module_child_maps mk
-- making child maps {{{2
-- TODO: do not export this
make_name_maps :: DeclChildrenList -> BoundValueList -> ADTVariantList -> WithErrors NameMaps
make_name_maps decls bound_values adt_variants =
    let decl_dups = find_dups decls
        bn_dups = find_dups bound_values
        variant_dups = find_dups adt_variants
    in report_dups decl_dups >> report_dups bn_dups >> report_dups variant_dups >>
    pure (NameMaps (make_map decls) (make_map bound_values) (make_map adt_variants))
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
make_name_maps_from_decls :: Arena.Arena (Type.TypeSynonym ty) Type.TypeSynonymKey -> [SIR.Binding stage] -> [Type.ADTKey] -> [Type.TypeSynonymKey] -> NRReader (Arena.Arena (Type.ADT (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage)) Type.ADTKey) (Arena.Arena (SIR.BoundValue stage) SIR.BoundValueKey) type_var_arena sir_child_maps WithErrors NameMaps
make_name_maps_from_decls type_synonym_arena bindings adts type_synonyms =
    unzip3 <$> mapM binding_children bindings >>= \ (binding_decl_entries, binding_bv_entries, binding_variant_entries) ->
    ask_adt_arena >>= \ adt_arena ->
    let (adt_decl_entries, adt_bv_entries, adt_variant_entries) =
            adts
                & map
                    (\ adt ->
                        let (Type.ADT _ (Located name_sp name) _ _) = Arena.get adt_arena adt
                            adt_decl_key = SIR.Decl'Type $ Type.Type'ADT adt []
                        in ([(name, DeclAt name_sp, adt_decl_key)], [], []) -- constructor bvs and variants handled by adt variant bindings, TODO: make it not the case so that this can deal with named variants too
                    )
                & unzip3
        (type_synonym_decl_entries, type_synonym_bv_entries, type_synonym_variant_entries) =
            type_synonyms
                & map
                    (\ synonym ->
                        let (Type.TypeSynonym _ (Located name_sp name) _) = Arena.get type_synonym_arena synonym
                            synonym_decl_key = SIR.Decl'Type $ Type.Type'Synonym synonym
                        in ([(name, DeclAt name_sp, synonym_decl_key)], [], [])
                    )
                & unzip3
    in lift $ make_name_maps
            (concat $ binding_decl_entries ++ adt_decl_entries ++ type_synonym_decl_entries)
            (concat $ binding_bv_entries ++ adt_bv_entries ++ type_synonym_bv_entries)
            (concat $ binding_variant_entries ++ adt_variant_entries ++ type_synonym_variant_entries)

collect_child_maps :: SIR.SIR stage -> WithErrors SIRChildMaps
collect_child_maps (SIR.SIR mod_arena adt_arena type_synonym_arena _ bound_value_arena _) = SIRChildMaps <$> Arena.transformM go mod_arena
    where
        primitive_decls =
                [ ("int", ImplicitPrim, SIR.Decl'Type Type.Type'Int)
                , ("float", ImplicitPrim, SIR.Decl'Type Type.Type'Float)
                , ("char", ImplicitPrim, SIR.Decl'Type Type.Type'Char)
                , ("string", ImplicitPrim, SIR.Decl'Type Type.Type'String)
                , ("bool", ImplicitPrim, SIR.Decl'Type Type.Type'Bool)
                ]
        primitive_bvs = []

        go (SIR.Module _ bindings adts type_synonyms) =
            -- TODO: remove runReaderT
            -- TODO: use make_name_maps_from_decls but somehow add primitives
            runReaderT (unzip3 <$> mapM binding_children bindings) (adt_arena, bound_value_arena, (), ()) >>= \ (binding_decl_entries, binding_bv_entries, binding_variant_entries) ->
            let (adt_decl_entries, adt_bv_entries, adt_variant_entries) =
                    adts
                        & map
                            (\ adt ->
                                let (Type.ADT _ (Located name_sp name) _ _) = Arena.get adt_arena adt
                                    adt_decl_key = SIR.Decl'Type $ Type.Type'ADT adt []
                                in ([(name, DeclAt name_sp, adt_decl_key)], [], [])) -- constructor bvs and variants handled by adt variant bindings, TODO: make it not the case so that this can deal with named variants too
                        & unzip3

                (type_synonym_decl_entries, type_synonym_bv_entries, type_synonym_variant_entries) =
                    type_synonyms
                        & map
                            (\ synonym ->
                                let (Type.TypeSynonym _ (Located name_sp name) _) = Arena.get type_synonym_arena synonym
                                    synonym_decl_key = SIR.Decl'Type $ Type.Type'Synonym synonym
                                in ([(name, DeclAt name_sp, synonym_decl_key)], [], []))
                        & unzip3
            in name_maps_to_child_maps
                <$> (make_name_maps
                        (concat $ primitive_decls : binding_decl_entries ++ adt_decl_entries ++ type_synonym_decl_entries)
                        (concat $ primitive_bvs : binding_bv_entries ++ adt_bv_entries ++ type_synonym_bv_entries)
                        (concat $ binding_variant_entries ++ adt_variant_entries ++ type_synonym_variant_entries))

name_maps_to_child_maps :: NameMaps -> ChildMaps
name_maps_to_child_maps (NameMaps decl val adtv) = ChildMaps decl val adtv
child_maps_to_name_maps :: ChildMaps -> NameMaps
child_maps_to_name_maps (ChildMaps decl val adtv) = NameMaps decl val adtv

binding_children :: Monad under => SIR.Binding stage -> NRReader (Arena.Arena (Type.ADT (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage)) Type.ADTKey) (Arena.Arena (SIR.BoundValue stage) SIR.BoundValueKey) type_var_arena sir_child_maps under (DeclChildrenList, BoundValueList, ADTVariantList)
binding_children (SIR.Binding pat _ _) = ([],, []) <$> pattern_bvs pat
binding_children (SIR.Binding'ADTVariant sp bvk _ variant_index) = bv_name bvk >>= \ name -> pure ([], [(name, DeclAt sp, bvk)], [(name, DeclAt sp, variant_index)]) -- TODO: move variants to inside their types, also dont handle adt variants here

pattern_bvs :: Monad under => SIR.Pattern stage -> NRReader (Arena.Arena (Type.ADT (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage)) Type.ADTKey) (Arena.Arena (SIR.BoundValue stage) SIR.BoundValueKey) type_var_arena sir_child_maps under BoundValueList
pattern_bvs (SIR.Pattern'Identifier _ sp bvk) = bv_name bvk >>= \ name -> pure [(name, DeclAt sp, bvk)]
pattern_bvs (SIR.Pattern'Wildcard _ _) = pure []
pattern_bvs (SIR.Pattern'Tuple _ _ a b) = pattern_bvs a >>= \ a -> pattern_bvs b >>= \ b -> pure (a ++ b)
pattern_bvs (SIR.Pattern'Named _ _ _ (Located bv_span bvk) subpat) = bv_name bvk >>= \ name -> pattern_bvs subpat >>= \ subpat -> pure ((name, DeclAt bv_span, bvk) : subpat)
pattern_bvs (SIR.Pattern'AnonADTVariant _ _ _ _ _ fields) = concat <$> mapM pattern_bvs fields
pattern_bvs (SIR.Pattern'NamedADTVariant _ _ _ _ _ fields) = concat <$> mapM (pattern_bvs . snd) fields
pattern_bvs (SIR.Pattern'Poison _ _) = pure []

bv_name :: Monad under => SIR.BoundValueKey -> NRReader (Arena.Arena (Type.ADT (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage)) Type.ADTKey) (Arena.Arena (SIR.BoundValue stage) SIR.BoundValueKey) type_var_arena sir_child_maps under Text
bv_name bvk =
    ask_bv_arena >>= \ bv_arena ->
    case Arena.get bv_arena bvk of
        SIR.BoundValue _ _ (Located _ name) -> pure name
        SIR.BoundValue'ADTVariant _ variant_index _ _ _ ->
            ask_adt_arena >>= \ adt_arena ->
            let variant = Type.get_adt_variant adt_arena variant_index
            in pure $ unlocate $ Type.variant_name variant

-- getting from child maps {{{2
-- TODO: remove duplication from these
get_decl_child :: SIRChildMaps -> SIR.Decl -> Located Text -> Either Error SIR.Decl
get_decl_child sir_child_maps decl name =
    let res = case decl of
            SIR.Decl'Module m ->
                let ChildMaps d_children _ _ = get_module_child_maps sir_child_maps m
                in Map.lookup (unlocate name) d_children

            SIR.Decl'Type _ -> Nothing
    in case res of
        Just res -> Right res
        Nothing -> Left $ Error'CouldNotFindIn Nothing name -- TODO: put previous

get_value_child :: SIRChildMaps -> SIR.Decl -> Located Text -> Either Error SIR.BoundValueKey
get_value_child sir_child_maps decl name =
    let res = case decl of
            SIR.Decl'Module m ->
                let ChildMaps _ v_children _ = get_module_child_maps sir_child_maps m
                in Map.lookup (unlocate name) v_children

            SIR.Decl'Type _ -> Nothing
    in case res of
        Just res -> Right res
        Nothing -> Left $ Error'CouldNotFindIn Nothing name -- TODO: put previous

get_variant_child :: SIRChildMaps -> SIR.Decl -> Located Text -> Either Error Type.ADTVariantIndex
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
type NRReader adt_arena bv_arena type_var_arena sir_child_maps = ReaderT (adt_arena, bv_arena, type_var_arena, sir_child_maps)

ask_adt_arena :: Applicative under => NRReader adt_arena bv_arena type_var_arena sir_child_maps under adt_arena
ask_adt_arena = ReaderT $ \ (adts, _, _, _) -> pure adts
ask_bv_arena :: Applicative under => NRReader adt_arena bv_arena type_var_arena sir_child_maps under bv_arena
ask_bv_arena = ReaderT $ \ (_, bvs, _, _) -> pure bvs
ask_type_var_arena :: Applicative under => NRReader adt_arena bv_arena type_var_arena sir_child_maps under type_var_arena
ask_type_var_arena = ReaderT $ \ (_, _, tvars, _) -> pure tvars
ask_sir_child_maps :: Applicative under => NRReader adt_arena bv_arena type_var_arena sir_child_maps under sir_child_maps
ask_sir_child_maps = ReaderT $ \ (_, _, _, sir_child_maps) -> pure sir_child_maps
