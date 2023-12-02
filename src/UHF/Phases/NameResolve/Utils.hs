module UHF.Phases.NameResolve.Utils
    ( Error (..)

    , ChildMaps (..)
    , ChildMapStack (..)
    , ModuleChildMaps

    , DeclAt (..)
    , DeclChildrenList
    , BoundValueList
    , ADTVariantList
    , make_child_maps
    , collect_child_maps

    , get_decl_child
    , get_value_child
    , get_variant_child

    , pattern_bvs
    , binding_children

    , CollectingErrors
    , NRReader
    , MakeDeclState
    , DeclArena

    , new_decl
    , ask_decl_arena

    , ask_adt_arena
    , ask_bv_arena
    , ask_type_var_arena
    , ask_module_child_maps
    ) where

-- TODO: clean up this and the 3 modules too

import UHF.Prelude

import qualified Data.List as List
import qualified Data.Map as Map

import UHF.Source.Located (Located (Located, unlocate))
import UHF.Source.Span (Span)
import qualified UHF.Util.Arena as Arena
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.SIR as SIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Diagnostic as Diagnostic

-- utilities for name resolution / phases that somewhat do name resolution
-- it is under the NameResolve module but is used by ResolveReferStarts, EvalTypeExprs, and NameResolve

-- error {{{1
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

-- child maps {{{1
data ChildMaps = ChildMaps (Map.Map Text SIR.DeclKey) (Map.Map Text SIR.BoundValueKey) (Map.Map Text Type.ADTVariantIndex) deriving Show
data ChildMapStack = ChildMapStack ChildMaps (Maybe ChildMapStack)
type ModuleChildMaps = Arena.Arena ChildMaps SIR.ModuleKey

data DeclAt = DeclAt Span | ImplicitPrim deriving Show

type DeclChildrenList = [(Text, DeclAt, SIR.DeclKey)]
type BoundValueList = [(Text, DeclAt, SIR.BoundValueKey)]
type ADTVariantList = [(Text, DeclAt, Type.ADTVariantIndex)]

-- making child maps {{{2
make_child_maps :: DeclChildrenList -> BoundValueList -> ADTVariantList -> CollectingErrors ChildMaps
make_child_maps decls bound_values adt_variants =
    let decl_dups = find_dups decls
        bn_dups = find_dups bound_values
        variant_dups = find_dups adt_variants
    in report_dups decl_dups >> report_dups bn_dups >> report_dups variant_dups >>
    pure (ChildMaps (make_map decls) (make_map bound_values) (make_map adt_variants))
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

binding_children :: Monad under => SIR.Binding stage -> NRReader (Arena.Arena (Type.ADT (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage)) Type.ADTKey) (Arena.Arena (SIR.BoundValue stage) SIR.BoundValueKey) type_var_arena module_child_maps under (DeclChildrenList, BoundValueList, ADTVariantList)
binding_children (SIR.Binding pat _ _) = ([],, []) <$> pattern_bvs pat
binding_children (SIR.Binding'ADTVariant sp bvk _ variant_index) = bv_name bvk >>= \ name -> pure ([], [(name, DeclAt sp, bvk)], [(name, DeclAt sp, variant_index)]) -- TODO: move variants to inside their types, also dont handle adt variants here

pattern_bvs :: Monad under => SIR.Pattern stage -> NRReader (Arena.Arena (Type.ADT (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage)) Type.ADTKey) (Arena.Arena (SIR.BoundValue stage) SIR.BoundValueKey) type_var_arena module_child_maps under BoundValueList
pattern_bvs (SIR.Pattern'Identifier _ sp bvk) = bv_name bvk >>= \ name -> pure [(name, DeclAt sp, bvk)]
pattern_bvs (SIR.Pattern'Wildcard _ _) = pure []
pattern_bvs (SIR.Pattern'Tuple _ _ a b) = pattern_bvs a >>= \ a -> pattern_bvs b >>= \ b -> pure (a ++ b)
pattern_bvs (SIR.Pattern'Named _ _ _ (Located bv_span bvk) subpat) = bv_name bvk >>= \ name -> pattern_bvs subpat >>= \ subpat -> pure ((name, DeclAt bv_span, bvk) : subpat)
pattern_bvs (SIR.Pattern'AnonADTVariant _ _ _ _ _ fields) = concat <$> mapM pattern_bvs fields
pattern_bvs (SIR.Pattern'NamedADTVariant _ _ _ _ _ fields) = concat <$> mapM (pattern_bvs . snd) fields
pattern_bvs (SIR.Pattern'Poison _ _) = pure []

bv_name :: Monad under => SIR.BoundValueKey -> NRReader (Arena.Arena (Type.ADT (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage)) Type.ADTKey) (Arena.Arena (SIR.BoundValue stage) SIR.BoundValueKey) type_var_arena module_child_maps under Text
bv_name bvk =
    ask_bv_arena >>= \ bv_arena ->
    case Arena.get bv_arena bvk of
        SIR.BoundValue _ _ (Located _ name) -> pure name
        SIR.BoundValue'ADTVariant _ variant_index _ _ _ ->
            ask_adt_arena >>= \ adt_arena ->
            let variant = Type.get_adt_variant adt_arena variant_index
            in pure $ unlocate $ Type.variant_name variant

-- TODO: do not run this multiple times in the multiple phases
collect_child_maps :: Arena.Arena (SIR.Module stage) SIR.ModuleKey -> Arena.Arena (Type.TypeSynonym (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage)) Type.TypeSynonymKey -> NRReader (Arena.Arena (Type.ADT (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage)) Type.ADTKey) (Arena.Arena (SIR.BoundValue stage) SIR.BoundValueKey) type_var_arena module_child_maps (MakeDeclState CollectingErrors) (Arena.Arena ChildMaps SIR.ModuleKey)
collect_child_maps mod_arena type_synonym_arena = Arena.transformM go mod_arena
    where
        primitive_decls =
            new_decl (SIR.Decl'Type Type.Type'Int) >>= \ int ->
            new_decl (SIR.Decl'Type Type.Type'Float) >>= \ float ->
            new_decl (SIR.Decl'Type Type.Type'Char) >>= \ char ->
            new_decl (SIR.Decl'Type Type.Type'String) >>= \ string ->
            new_decl (SIR.Decl'Type Type.Type'Bool) >>= \ bool ->
            pure
                [ ("int", ImplicitPrim, int)
                , ("float", ImplicitPrim, float)
                , ("char", ImplicitPrim, char)
                , ("string", ImplicitPrim, string)
                , ("bool", ImplicitPrim, bool)
                ]
        primitive_bvs = pure []

        go (SIR.Module _ bindings adts type_synonyms) =
            (unzip3 <$> mapM binding_children bindings) >>= \ (binding_decl_entries, binding_bv_entries, binding_variant_entries) ->
            unzip3 <$>
                mapM
                    (\ adt ->
                        ask_adt_arena >>= \ adt_arena ->
                        let (Type.ADT _ (Located name_sp name) _ _) = Arena.get adt_arena adt
                        in lift (new_decl (SIR.Decl'Type $ Type.Type'ADT adt [])) >>= \ adt_decl_key ->
                        pure ([(name, DeclAt name_sp, adt_decl_key)], [], []) -- constructor bvs and variants handled by adt variant bindings, TODO: make it not the case so that this can deal with named variants too
                    )
                    adts >>= \ (adt_decl_entries, adt_bv_entries, adt_variant_entries) ->
            unzip3 <$>
                mapM
                    (\ synonym ->
                        let (Type.TypeSynonym _ (Located name_sp name) _) = Arena.get type_synonym_arena synonym
                        in lift (new_decl (SIR.Decl'Type $ Type.Type'Synonym synonym)) >>= \ synonym_decl_key ->
                        pure ([(name, DeclAt name_sp, synonym_decl_key)], [], [])
                    )
                    type_synonyms >>= \ (type_synonym_decl_entries, type_synonym_bv_entries, type_synonym_variant_entries) ->
            lift primitive_decls >>= \ primitive_decls ->
            lift primitive_bvs >>= \ primitive_bvs ->
            lift (lift $ make_child_maps
                (concat $ primitive_decls : binding_decl_entries ++ adt_decl_entries ++ type_synonym_decl_entries)
                (concat $ primitive_bvs : binding_bv_entries ++ adt_bv_entries ++ type_synonym_bv_entries)
                (concat $ binding_variant_entries ++ adt_variant_entries ++ type_synonym_variant_entries))
-- getting from child maps {{{2
-- TODO: remove duplication from tehse
get_decl_child :: DeclArena -> ModuleChildMaps -> SIR.DeclKey -> Located Text -> Either Error SIR.DeclKey
get_decl_child decls mods thing name =
    let res = case Arena.get decls thing of
            SIR.Decl'Module m ->
                let (ChildMaps d_children _ _) = Arena.get mods m
                in Map.lookup (unlocate name) d_children

            SIR.Decl'Type _ -> Nothing
    in case res of
        Just res -> Right res
        Nothing -> Left $ Error'CouldNotFindIn Nothing name -- TODO: put previous

get_value_child :: DeclArena -> ModuleChildMaps -> SIR.DeclKey -> Located Text -> Either Error SIR.BoundValueKey
get_value_child decls mods thing name =
    let res = case Arena.get decls thing of
            SIR.Decl'Module m ->
                let ChildMaps _ v_children _ = Arena.get mods m
                in Map.lookup (unlocate name) v_children

            SIR.Decl'Type _ -> Nothing
    in case res of
        Just res -> Right res
        Nothing -> Left $ Error'CouldNotFindIn Nothing name -- TODO: put previous

get_variant_child :: DeclArena -> ModuleChildMaps -> SIR.DeclKey -> Located Text -> Either Error Type.ADTVariantIndex
get_variant_child decls mods thing name =
    let res = case Arena.get decls thing of
            SIR.Decl'Module m ->
                let ChildMaps _ _ adtv_children = Arena.get mods m
                in Map.lookup (unlocate name) adtv_children

            SIR.Decl'Type _ -> Nothing
    in case res of
        Just res -> Right res
        Nothing -> Left $ Error'CouldNotFindIn Nothing name -- TODO: put previous
-- monads {{{1
type CollectingErrors = Compiler.WithDiagnostics Error Void
type NRReader adt_arena bv_arena type_var_arena module_child_maps = ReaderT (adt_arena, bv_arena, type_var_arena, module_child_maps)
type MakeDeclState = StateT DeclArena
-- TODO: instead of decl arena, just have a data Decl = ADT ADTKey | TS TypeSynonymKey or something?, remove SIR.Decl?
type DeclArena = Arena.Arena SIR.Decl SIR.DeclKey

new_decl :: Monad under => SIR.Decl -> MakeDeclState under SIR.DeclKey
new_decl d = StateT (\ arena -> pure $ Arena.put d arena)
ask_decl_arena :: Applicative under => MakeDeclState under DeclArena
ask_decl_arena = StateT $ \ arena -> pure (arena, arena)

ask_adt_arena :: Applicative under => NRReader adt_arena bv_arena type_var_arena module_child_maps under adt_arena
ask_adt_arena = ReaderT $ \ (adts, _, _, _) -> pure adts
ask_bv_arena :: Applicative under => NRReader adt_arena bv_arena type_var_arena module_child_maps under bv_arena
ask_bv_arena = ReaderT $ \ (_, bvs, _, _) -> pure bvs
ask_type_var_arena :: Applicative under => NRReader adt_arena bv_arena type_var_arena module_child_maps under type_var_arena
ask_type_var_arena = ReaderT $ \ (_, _, tvars, _) -> pure tvars
ask_module_child_maps :: Applicative under => NRReader adt_arena bv_arena type_var_arena module_child_maps under module_child_maps
ask_module_child_maps = ReaderT $ \ (_, _, _, mcms) -> pure mcms
