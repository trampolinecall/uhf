module UHF.Parts.NameResolve.NameMaps
    ( NameMapStackKey
    , NameMaps (..)
    , NameMapStack (..)
    , ChildMaps
    , empty_name_maps
    , empty_child_maps

    , DeclAt (..)
    , DeclChildrenList
    , ValueList
    , ADTVariantList

    , SIRChildMaps
    , empty_sir_child_maps
    , get_module_child_maps

    , get_decl_child
    , get_value_child
    , get_variant_child

    , pattern_vars
    , binding_children
    ) where

-- TODO: clean up this and the 3 modules too
-- TODO: clean up folds

import UHF.Prelude

import qualified Data.List as List
import qualified Data.Map as Map

import qualified GHC.Enum as Enum (enumFromTo, minBound, maxBound)
import UHF.Parts.NameResolve.DeclAt
import UHF.Parts.NameResolve.Error
import UHF.Parts.NameResolve.NRReader
import UHF.Source.Located (Located (Located, unlocate))
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Intrinsics as Intrinsics
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.TypeSolver.TypeWithInferVar as TypeWithInferVar
import qualified UHF.Util.Arena as Arena

newtype NameMapStackKey = NameMapStackKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key NameMapStackKey where
    make_key = NameMapStackKey
    unmake_key (NameMapStackKey i) = i

-- ChilsMaps and NameMaps are very similar datatypes. the difference between them is conceptual: ChildMaps is a map that tells what the children of a
-- certain entity are, whereas a NameMap just stores what names are currently in scope (and is only used for resolving roots). in some cases the
-- distinction is actually important. consider for example an ADT:
--     data X #A #B #C {
--         ...
--     }
-- A, B, and C are all in scope inside of X so that variants and declarations inside X can use them, so they are part of the NameMap that represents
-- the names in scope inside of X, but A, B, and C are not part of the child maps of X, so you can't refer to X::A.
-- this is also why there is a NameMapStack but not a ChildMapStack.
-- TODO: rename NameMaps to NameMap and ChildMaps to ChildMap?
data ChildMaps = ChildMaps (Map.Map Text (SIR.DeclRef TypeWithInferVar.Type)) (Map.Map Text SIR.ValueRef) (Map.Map Text Type.ADT.VariantIndex) deriving Show
data NameMaps = NameMaps (Map.Map Text (SIR.DeclRef TypeWithInferVar.Type)) (Map.Map Text SIR.ValueRef) (Map.Map Text Type.ADT.VariantIndex) deriving Show
data NameMapStack = NameMapStack NameMaps (Maybe NameMapStackKey)

empty_name_maps :: NameMaps
empty_name_maps = NameMaps Map.empty Map.empty Map.empty

empty_child_maps :: ChildMaps
empty_child_maps = ChildMaps Map.empty Map.empty Map.empty

-- TODO: add_to_child_maps :: DeclChildrenList -> ValueList -> ADTVariantList -> ChildMaps -> Either DuplicateChild ChildMaps
-- TODO: add_to_name_maps :: DeclChildrenList -> ValueList -> ADTVariantList -> NameMaps -> Either DuplicateName NameMaps

-- TODO: do not export these
type DeclChildrenList = [(Text, DeclAt, SIR.DeclRef TypeWithInferVar.Type)]
type ValueList = [(Text, DeclAt, SIR.ValueRef)]
type ADTVariantList = [(Text, DeclAt, Type.ADT.VariantIndex)]

-- SIRChildMaps {{{1
newtype SIRChildMaps = SIRChildMaps (Arena.Arena ChildMaps SIR.ModuleKey)

empty_sir_child_maps :: SIR.SIR stage -> SIRChildMaps
empty_sir_child_maps (SIR.SIR mod_arena _ _ _ _ _) = SIRChildMaps $ Arena.transform go mod_arena
    where
        go (SIR.Module _ _ _ _) = empty_child_maps

get_module_child_maps :: SIRChildMaps -> SIR.ModuleKey -> ChildMaps
get_module_child_maps (SIRChildMaps module_child_maps) = Arena.get module_child_maps

-- TODO: add_to_module_child_maps :: DeclChildrenList -> ValueList -> ADTVariantList -> Arena.Arena NameMaps NameMapStackKey -> SIRChildMaps -> Either DuplicateName (SIRChildMaps, Arena.Arena NameMaps NameMapStackKey)
-- making child maps {{{1
-- TODO: do not export this
-- TODO: delete this (superseded by add_to_name_maps)
-- make_name_maps :: DeclChildrenList -> ValueList -> ADTVariantList -> WithErrors NameMaps
-- make_name_maps decls values adt_variants =
--     let decl_dups = find_dups decls
--         bn_dups = find_dups values
--         variant_dups = find_dups adt_variants
--     in report_dups decl_dups >> report_dups bn_dups >> report_dups variant_dups >>
--     pure (NameMaps (make_map decls) (make_map values) (make_map adt_variants))
--     where
--         -- separate finding duplicates from making maps so that if there is a duplicate the whole name context doesnt just disappear
--         -- duplicates will just take the last bound name in the last, because of the how Map.fromList is implemented
--         find_dups x =
--             let grouped = List.groupBy ((==) `on` get_name) $ List.sortBy (compare `on` get_name) x -- compare names of bound names only
--             in filter ((1/=) . length) grouped
--             where
--                 get_name (n, _, _) = n
--
--         make_map x = Map.fromList (map (\ (n, _, v) -> (n, v)) x)
--
--         report_dups = mapM_
--             (\ decls ->
--                 let (first_name, _, _) = head decls
--                 in Compiler.tell_error $ Error'MultipleDecls first_name (map get_decl_at decls))
--             where
--                 get_decl_at (_, d, _) = d

-- TODO: remove already arguments and find a better way to do things
-- TODO: remove this (or rewrite it as add_to_name_maps_from_decls?)
-- make_name_maps_from_decls :: DeclChildrenList -> ValueList -> ADTVariantList -> Arena.Arena (Type.TypeSynonym ty) Type.TypeSynonymKey -> [SIR.Binding stage] -> [Type.ADTKey] -> [Type.TypeSynonymKey] -> NRReader (Arena.Arena (Type.ADT (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage)) Type.ADTKey) (Arena.Arena (SIR.Variable stage) SIR.VariableKey) quant_var_arena sir_child_maps WithErrors NameMaps
-- make_name_maps_from_decls already_decls already_vals already_variants type_synonym_arena bindings adts type_synonyms =
--     unzip3 <$> mapM binding_children bindings >>= \ (binding_decl_entries, binding_val_entries, binding_variant_entries) ->
--     ask_adt_arena >>= \ adt_arena ->
--     let (adt_decl_entries, adt_val_entries, adt_variant_entries) =
--             adts
--                 & map
--                     (\ adt ->
--                         let (Type.ADT _ (Located adt_name_sp adt_name) _ _) = Arena.get adt_arena adt
--                             (variant_constructors, variant_patterns) = Type.ADT.variant_idxs adt_arena adt
--                                 & map (\ variant_index ->
--                                     case Type.ADT.get_variant adt_arena variant_index of
--                                         Type.ADT.Variant'Anon (Located variant_name_sp variant_name) _ _ -> ((variant_name, DeclAt variant_name_sp, SIR.ValueRef'ADTVariantConstructor variant_index), (variant_name, DeclAt variant_name_sp, variant_index))
--                                         Type.ADT.Variant'Named _ _ _ -> todo
--                                 )
--                                 & unzip
--
--                         in ([(adt_name, DeclAt adt_name_sp, SIR.DeclRef'Type $ TypeWithInferVar.Type'ADT adt [])], variant_constructors, variant_patterns)) -- TODO: make this deal with named variants too; also TODO: move variants to inside their types
--                 & unzip3
--
--         (type_synonym_decl_entries, type_synonym_val_entries, type_synonym_variant_entries) =
--             type_synonyms
--                 & map
--                     (\ synonym ->
--                         let (Type.TypeSynonym _ (Located name_sp name) _) = Arena.get type_synonym_arena synonym
--                             synonym_decl_key = SIR.DeclRef'Type $ TypeWithInferVar.Type'Synonym synonym
--                         in ([(name, DeclAt name_sp, synonym_decl_key)], [], [])
--                     )
--                 & unzip3
--     in lift $ make_name_maps
--             (concat $ already_decls : binding_decl_entries ++ adt_decl_entries ++ type_synonym_decl_entries)
--             (concat $ already_vals : binding_val_entries ++ adt_val_entries ++ type_synonym_val_entries)
--             (concat $ already_variants : binding_variant_entries ++ adt_variant_entries ++ type_synonym_variant_entries)

-- TODO: remove this
-- collect_child_maps :: SIR.SIR stage -> WithErrors SIRChildMaps
-- collect_child_maps (SIR.SIR mod_arena adt_arena type_synonym_arena _ variable_arena _) = SIRChildMaps <$> Arena.transformM go mod_arena
--     where
--         primitive_decls =
--                 [ ("int", ImplicitPrim, SIR.DeclRef'Type TypeWithInferVar.Type'Int)
--                 , ("float", ImplicitPrim, SIR.DeclRef'Type TypeWithInferVar.Type'Float)
--                 , ("char", ImplicitPrim, SIR.DeclRef'Type TypeWithInferVar.Type'Char)
--                 , ("string", ImplicitPrim, SIR.DeclRef'Type TypeWithInferVar.Type'String)
--                 , ("bool", ImplicitPrim, SIR.DeclRef'Type TypeWithInferVar.Type'Bool)
--                 , ("uhf_intrinsics", ImplicitPrim, SIR.DeclRef'ExternPackage SIR.ExternPackage'IntrinsicsPackage)
--                 ]
--         primitive_vals = []
--
--         go (SIR.Module _ bindings adts type_synonyms) =
--             runReaderT (name_maps_to_child_maps <$> make_name_maps_from_decls primitive_decls primitive_vals [] type_synonym_arena bindings adts type_synonyms) (adt_arena, variable_arena, (), ())

binding_children :: Monad under => SIR.Binding stage -> NRReader (Arena.Arena (Type.ADT (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage)) Type.ADTKey) (Arena.Arena (SIR.Variable stage) SIR.VariableKey) quant_var_arena sir_child_maps under (DeclChildrenList, ValueList, ADTVariantList)
binding_children (SIR.Binding pat _ _) = ([],, []) <$> pattern_vars pat

pattern_vars :: Monad under => SIR.Pattern stage -> NRReader (Arena.Arena (Type.ADT (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage)) Type.ADTKey) (Arena.Arena (SIR.Variable stage) SIR.VariableKey) quant_var_arena sir_child_maps under ValueList
pattern_vars (SIR.Pattern'Variable _ sp var_key) = var_name var_key >>= \ name -> pure [(name, DeclAt sp, SIR.ValueRef'Variable var_key)]
pattern_vars (SIR.Pattern'Wildcard _ _) = pure []
pattern_vars (SIR.Pattern'Tuple _ _ a b) = pattern_vars a >>= \ a -> pattern_vars b >>= \ b -> pure (a ++ b)
pattern_vars (SIR.Pattern'Named _ _ _ (Located var_span var_key) subpat) = var_name var_key >>= \ name -> pattern_vars subpat >>= \ subpat -> pure ((name, DeclAt var_span, SIR.ValueRef'Variable var_key) : subpat)
pattern_vars (SIR.Pattern'AnonADTVariant _ _ _ _ _ fields) = concat <$> mapM pattern_vars fields
pattern_vars (SIR.Pattern'NamedADTVariant _ _ _ _ _ fields) = concat <$> mapM (pattern_vars . snd) fields
pattern_vars (SIR.Pattern'Poison _ _) = pure []

-- TODO: not sure if this is the best way to do this
var_name :: Monad under => SIR.VariableKey -> NRReader (Arena.Arena (Type.ADT (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage)) Type.ADTKey) (Arena.Arena (SIR.Variable stage) SIR.VariableKey) quant_var_arena sir_child_maps under Text
var_name var_key =
    ask_var_arena >>= \ var_arena ->
    let SIR.Variable _ _ (Located _ name) = Arena.get var_arena var_key
    in pure name

-- intrinsics package child maps {{{1
-- not sure if this is the best place to put this
intrinsics_package_child_maps :: ChildMaps
intrinsics_package_child_maps =
    ChildMaps
        Map.empty
        (Map.fromList $ map (\ intrinsic -> (Intrinsics.intrinsic_bv_name intrinsic, SIR.ValueRef'Intrinsic intrinsic)) (Enum.enumFromTo Enum.minBound Enum.maxBound))
        Map.empty
-- getting from child maps {{{1
-- TODO: remove duplication from these
get_decl_child :: SIRChildMaps -> SIR.DeclRef TypeWithInferVar.Type -> Located Text -> Either Error (SIR.DeclRef TypeWithInferVar.Type)
get_decl_child sir_child_maps decl name =
    let res = case decl of
            SIR.DeclRef'Module m ->
                let ChildMaps d_children _ _ = get_module_child_maps sir_child_maps m
                in Map.lookup (unlocate name) d_children
            SIR.DeclRef'Type _ -> Nothing
            SIR.DeclRef'ExternPackage SIR.ExternPackage'IntrinsicsPackage ->
                let ChildMaps d_children _ _ = intrinsics_package_child_maps
                in Map.lookup (unlocate name) d_children
    in case res of
        Just res -> Right res
        Nothing -> Left $ Error'CouldNotFindIn Nothing name -- TODO: put previous
get_value_child :: SIRChildMaps -> SIR.DeclRef TypeWithInferVar.Type -> Located Text -> Either Error SIR.ValueRef
get_value_child sir_child_maps decl name =
    let res = case decl of
            SIR.DeclRef'Module m ->
                let ChildMaps _ v_children _ = get_module_child_maps sir_child_maps m
                in Map.lookup (unlocate name) v_children
            SIR.DeclRef'Type _ -> Nothing
            SIR.DeclRef'ExternPackage SIR.ExternPackage'IntrinsicsPackage ->
                let ChildMaps _ v_children _ = intrinsics_package_child_maps
                in Map.lookup (unlocate name) v_children
    in case res of
        Just res -> Right res
        Nothing -> Left $ Error'CouldNotFindIn Nothing name -- TODO: put previous
get_variant_child :: SIRChildMaps -> SIR.DeclRef TypeWithInferVar.Type -> Located Text -> Either Error Type.ADT.VariantIndex
get_variant_child sir_child_maps decl name =
    let res = case decl of
            SIR.DeclRef'Module m ->
                let ChildMaps _ _ adtv_children = get_module_child_maps sir_child_maps m
                in Map.lookup (unlocate name) adtv_children
            SIR.DeclRef'Type _ -> Nothing
            SIR.DeclRef'ExternPackage SIR.ExternPackage'IntrinsicsPackage ->
                let ChildMaps _ _ adtv_children = intrinsics_package_child_maps
                in Map.lookup (unlocate name) adtv_children
    in case res of
        Just res -> Right res
        Nothing -> Left $ Error'CouldNotFindIn Nothing name -- TODO: put previous
