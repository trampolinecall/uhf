module UHF.Parts.NameResolve.NameMaps
    ( NameMapStackKey
    , NameMapStack (..)
    , NameMaps
    , ChildMaps
    , DeclAt (..)
    , empty_child_maps
    , empty_name_maps
    , Child
    , DeclChild
    , ValueChild
    , ADTVariantChild
    , add_to_child_maps
    , add_to_name_maps
    , add_tuple_to_child_maps
    , add_tuple_to_name_maps
    , SIRChildMaps
    , empty_sir_child_maps
    , get_module_child_maps
    , add_to_module_child_maps
    , add_tuple_to_module_child_maps
    , look_up_decl
    , look_up_value
    , look_up_variant
    , get_decl_child
    , get_value_child
    , get_variant_child
    , decls_to_children
    , binding_to_children
    , pattern_to_children
    , quant_vars_to_children
    ) where

-- TODO: clean up this and the 3 modules too
-- TODO: clean up folds

import UHF.Prelude

import qualified Data.Map as Map

import qualified GHC.Enum as Enum (enumFromTo, maxBound, minBound)
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Intrinsics as Intrinsics
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import UHF.Parts.NameResolve.DeclAt
import UHF.Parts.NameResolve.Error
import UHF.Parts.NameResolve.NRReader
import qualified UHF.Parts.TypeSolver.TypeWithInferVar as TypeWithInferVar
import UHF.Source.Located (Located (Located, unlocate))
import qualified UHF.Util.Arena as Arena

-- ChildMaps and NameMaps {{{1
-- Maps is so that the functions that act on ChildMaps and NameMaps aren't so duplicated: by having ChildMaps and NameMaps be newtypes of Maps, we
-- can have 1 function that does some operation on Maps and then 2 exported functions that unwraps the ChildMaps and NameMaps newtypes and applies
-- that 1 function to the underlying Maps instead of having 2 almost identical functions for both ChildMaps and NameMaps
data Maps
    = Maps (Map.Map Text (DeclAt, SIR.DeclRef TypeWithInferVar.Type)) (Map.Map Text (DeclAt, SIR.ValueRef)) (Map.Map Text (DeclAt, Type.ADT.VariantIndex))
    deriving Show

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
newtype ChildMaps = ChildMaps Maps deriving Show
newtype NameMaps = NameMaps Maps deriving Show

empty_maps :: Maps
empty_maps = Maps Map.empty Map.empty Map.empty

empty_name_maps :: NameMaps
empty_name_maps = NameMaps empty_maps

empty_child_maps :: ChildMaps
empty_child_maps = ChildMaps empty_maps

type Child resolved = (Text, DeclAt, resolved)
type DeclChild = Child (SIR.DeclRef TypeWithInferVar.Type)
type ValueChild = Child SIR.ValueRef
type ADTVariantChild = Child Type.ADT.VariantIndex

add_to_maps :: [DeclChild] -> [ValueChild] -> [ADTVariantChild] -> Maps -> WithErrors Maps
add_to_maps decls values adt_variants maps = do
    maps <- foldlM add_decl maps decls
    maps <- foldlM add_value maps values
    maps <- foldlM add_adt_variant maps adt_variants
    pure maps
    where
        add_decl (Maps decls values adt_variants) decl = do
            decls <- add decls decl
            pure $ Maps decls values adt_variants
        add_value (Maps decls values adt_variants) value = do
            values <- add values value
            pure $ Maps decls values adt_variants
        add_adt_variant (Maps decls values adt_variants) adt_variant = do
            adt_variants <- add adt_variants adt_variant
            pure $ Maps decls values adt_variants

        add :: Map Text (DeclAt, thing) -> Child thing -> WithErrors (Map Text (DeclAt, thing))
        add map (name, decl_at, resolved) =
            case Map.lookup name map of
                Just (prev_decl_at, _) -> do
                    _ <- Compiler.tell_error $ Error'DuplicateDecl name prev_decl_at decl_at
                    pure map
                Nothing -> pure $ Map.insert name (decl_at, resolved) map

add_to_child_maps :: [DeclChild] -> [ValueChild] -> [ADTVariantChild] -> ChildMaps -> WithErrors ChildMaps
add_to_child_maps ds vs as (ChildMaps m) = ChildMaps <$> add_to_maps ds vs as m
add_to_name_maps :: [DeclChild] -> [ValueChild] -> [ADTVariantChild] -> NameMaps -> WithErrors NameMaps
add_to_name_maps ds vs as (NameMaps m) = NameMaps <$> add_to_maps ds vs as m

add_tuple_to_child_maps :: ([DeclChild], [ValueChild], [ADTVariantChild]) -> ChildMaps -> WithErrors ChildMaps
add_tuple_to_child_maps (ds, vs, as) (ChildMaps m) = ChildMaps <$> add_to_maps ds vs as m
add_tuple_to_name_maps :: ([DeclChild], [ValueChild], [ADTVariantChild]) -> NameMaps -> WithErrors NameMaps
add_tuple_to_name_maps (ds, vs, as) (NameMaps m) = NameMaps <$> add_to_maps ds vs as m

-- NameMapStack {{{1
data NameMapStack = NameMapStack NameMaps (Maybe NameMapStackKey)
newtype NameMapStackKey = NameMapStackKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key NameMapStackKey where
    make_key = NameMapStackKey
    unmake_key (NameMapStackKey i) = i

-- SIRChildMaps {{{1
newtype SIRChildMaps = SIRChildMaps (Arena.Arena ChildMaps SIR.ModuleKey)

empty_sir_child_maps :: SIR.SIR stage -> SIRChildMaps
empty_sir_child_maps (SIR.SIR mod_arena _ _ _ _ _) = SIRChildMaps $ Arena.transform go mod_arena
    where
        go (SIR.Module _ _ _ _ _) = empty_child_maps

get_module_child_maps :: SIRChildMaps -> SIR.ModuleKey -> ChildMaps
get_module_child_maps (SIRChildMaps module_child_maps) = Arena.get module_child_maps

add_to_module_child_maps :: [DeclChild] -> [ValueChild] -> [ADTVariantChild] -> SIR.ModuleKey -> SIRChildMaps -> WithErrors SIRChildMaps
add_to_module_child_maps ds vs as mod_key (SIRChildMaps module_child_maps) = SIRChildMaps <$> Arena.modifyM module_child_maps mod_key (add_to_child_maps ds vs as)

add_tuple_to_module_child_maps :: ([DeclChild], [ValueChild], [ADTVariantChild]) -> SIR.ModuleKey -> SIRChildMaps -> WithErrors SIRChildMaps
add_tuple_to_module_child_maps (ds, vs, as) mod_key (SIRChildMaps module_child_maps) = SIRChildMaps <$> Arena.modifyM module_child_maps mod_key (add_to_child_maps ds vs as)

-- getting from name maps and child maps {{{1
look_up_decl :: Arena.Arena NameMapStack NameMapStackKey -> NameMapStackKey -> Located Text -> Either Error (SIR.DeclRef TypeWithInferVar.Type)
look_up_decl = look_up (\ (NameMaps (Maps d _ _)) -> d)
look_up_value :: Arena.Arena NameMapStack NameMapStackKey -> NameMapStackKey -> Located Text -> Either Error SIR.ValueRef
look_up_value = look_up (\ (NameMaps (Maps _ val _)) -> val)
look_up_variant :: Arena.Arena NameMapStack NameMapStackKey -> NameMapStackKey -> Located Text -> Either Error Type.ADT.VariantIndex
look_up_variant = look_up (\ (NameMaps (Maps _ _ var)) -> var)

-- TODO: make this able to return inconclusive results (because macros)
look_up :: (NameMaps -> Map Text (DeclAt, result)) -> Arena.Arena NameMapStack NameMapStackKey -> NameMapStackKey -> Located Text -> Either Error result
look_up which_map arena name_map_stack iden =
    go name_map_stack
    where
        go current_name_map_stack_key =
            let NameMapStack current_name_maps parent = Arena.get arena current_name_map_stack_key
            in case Map.lookup (unlocate iden) (which_map current_name_maps) of
                Just (_, decl) -> Right decl
                Nothing ->
                    case parent of
                        Just parent -> go parent
                        Nothing -> Left $ Error'CouldNotFind iden

get_decl_child :: SIRChildMaps -> SIR.DeclRef TypeWithInferVar.Type -> Located Text -> Either Error (SIR.DeclRef TypeWithInferVar.Type)
get_decl_child sir_child_maps decl name =
    let res = case decl of
            SIR.DeclRef'Module m ->
                let ChildMaps (Maps d_children _ _) = get_module_child_maps sir_child_maps m
                in Map.lookup (unlocate name) d_children
            SIR.DeclRef'Type _ -> Nothing
            SIR.DeclRef'ExternPackage SIR.ExternPackage'IntrinsicsPackage ->
                let ChildMaps (Maps d_children _ _) = intrinsics_package_child_maps
                in Map.lookup (unlocate name) d_children
    in case res of
        Just (_, res) -> Right res
        Nothing -> Left $ Error'CouldNotFindIn Nothing name -- TODO: put previous
get_value_child :: SIRChildMaps -> SIR.DeclRef TypeWithInferVar.Type -> Located Text -> Either Error SIR.ValueRef
get_value_child sir_child_maps decl name =
    let res = case decl of
            SIR.DeclRef'Module m ->
                let ChildMaps (Maps _ v_children _) = get_module_child_maps sir_child_maps m
                in Map.lookup (unlocate name) v_children
            SIR.DeclRef'Type _ -> Nothing
            SIR.DeclRef'ExternPackage SIR.ExternPackage'IntrinsicsPackage ->
                let ChildMaps (Maps _ v_children _) = intrinsics_package_child_maps
                in Map.lookup (unlocate name) v_children
    in case res of
        Just (_, res) -> Right res
        Nothing -> Left $ Error'CouldNotFindIn Nothing name -- TODO: put previous
get_variant_child :: SIRChildMaps -> SIR.DeclRef TypeWithInferVar.Type -> Located Text -> Either Error Type.ADT.VariantIndex
get_variant_child sir_child_maps decl name =
    let res = case decl of
            SIR.DeclRef'Module m ->
                let ChildMaps (Maps _ _ adtv_children) = get_module_child_maps sir_child_maps m
                in Map.lookup (unlocate name) adtv_children
            SIR.DeclRef'Type _ -> Nothing
            SIR.DeclRef'ExternPackage SIR.ExternPackage'IntrinsicsPackage ->
                let ChildMaps (Maps _ _ adtv_children) = intrinsics_package_child_maps
                in Map.lookup (unlocate name) adtv_children
    in case res of
        Just (_, res) -> Right res
        Nothing -> Left $ Error'CouldNotFindIn Nothing name -- TODO: put previous

-- utilities for adding things to name maps and child maps {{{1
decls_to_children ::
    Monad under =>
    [SIR.Binding stage] ->
    [Type.ADTKey] ->
    [Type.TypeSynonymKey] ->
    NRReader
        (Arena.Arena (Type.ADT (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage)) Type.ADTKey)
        (Arena.Arena (Type.TypeSynonym (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage)) Type.TypeSynonymKey)
        (Arena.Arena (SIR.Variable stage) SIR.VariableKey)
        quant_var_arena
        sir_child_maps
        under
        ([DeclChild], [ValueChild], [ADTVariantChild])
decls_to_children bindings adts type_synonyms = do
    adt_arena <- ask_adt_arena
    let (adt_decl_entries, adt_val_entries, adt_variant_entries) =
            adts
                & map
                    ( \adt ->
                        let (Type.ADT _ (Located adt_name_sp adt_name) _ _) = Arena.get adt_arena adt
                            (variant_constructors, variant_patterns) =
                                Type.ADT.variant_idxs adt_arena adt
                                    & map
                                        ( \variant_index ->
                                            case Type.ADT.get_variant adt_arena variant_index of
                                                Type.ADT.Variant'Anon (Located variant_name_sp variant_name) _ _ -> ((variant_name, DeclAt variant_name_sp, SIR.ValueRef'ADTVariantConstructor variant_index), (variant_name, DeclAt variant_name_sp, variant_index))
                                                Type.ADT.Variant'Named _ _ _ -> todo
                                        )
                                    & unzip
                        in ([(adt_name, DeclAt adt_name_sp, SIR.DeclRef'Type $ TypeWithInferVar.Type'ADT adt [])], variant_constructors, variant_patterns) -- TODO: make this deal with named variants too; also TODO: move variants to inside their types
                    )
                & unzip3

    type_synonym_arena <- ask_type_synonym_arena
    let (type_synonym_decl_entries, type_synonym_val_entries, type_synonym_variant_entries) =
            type_synonyms
                & map
                    ( \synonym ->
                        let (Type.TypeSynonym _ (Located name_sp name) _) = Arena.get type_synonym_arena synonym
                            synonym_decl_key = SIR.DeclRef'Type $ TypeWithInferVar.Type'Synonym synonym
                        in ([(name, DeclAt name_sp, synonym_decl_key)], [], [])
                    )
                & unzip3

    (binding_decl_entries, binding_val_entries, binding_variant_entries) <- unzip3 <$> mapM binding_to_children bindings

    pure
        ( concat $ binding_decl_entries ++ adt_decl_entries ++ type_synonym_decl_entries
        , concat $ binding_val_entries ++ adt_val_entries ++ type_synonym_val_entries
        , concat $ binding_variant_entries ++ adt_variant_entries ++ type_synonym_variant_entries
        )

binding_to_children ::
    Monad under =>
    SIR.Binding stage ->
    NRReader
        adt_arena
        type_synonym_arena
        (Arena.Arena (SIR.Variable stage) SIR.VariableKey)
        quant_var_arena
        sir_child_maps
        under
        ([DeclChild], [ValueChild], [ADTVariantChild])
binding_to_children (SIR.Binding pat _ _) = ([],,[]) <$> pattern_to_children pat

pattern_to_children ::
    Monad under =>
    SIR.Pattern stage ->
    NRReader adt_arena type_synonym_arena (Arena.Arena (SIR.Variable stage) SIR.VariableKey) quant_var_arena sir_child_maps under [ValueChild]
pattern_to_children (SIR.Pattern'Variable _ sp var_key) = do
    name <- var_name var_key
    pure [(name, DeclAt sp, SIR.ValueRef'Variable var_key)]
pattern_to_children (SIR.Pattern'Wildcard _ _) = pure []
pattern_to_children (SIR.Pattern'Tuple _ _ a b) = do
    a' <- pattern_to_children a
    b' <- pattern_to_children b
    pure $ a' ++ b'
pattern_to_children (SIR.Pattern'Named _ _ _ (Located var_span var_key) subpat) = do
    name <- var_name var_key
    subpat' <- pattern_to_children subpat
    pure ((name, DeclAt var_span, SIR.ValueRef'Variable var_key) : subpat')
pattern_to_children (SIR.Pattern'AnonADTVariant _ _ _ _ _ fields) = concat <$> mapM pattern_to_children fields
pattern_to_children (SIR.Pattern'NamedADTVariant _ _ _ _ _ fields) = concat <$> mapM (pattern_to_children . snd) fields
pattern_to_children (SIR.Pattern'Poison _ _) = pure []
var_name ::
    Monad under =>
    SIR.VariableKey ->
    NRReader adt_arena type_synonym_arena (Arena.Arena (SIR.Variable stage) SIR.VariableKey) quant_var_arena sir_child_maps under Text
var_name var_key = do
    var_arena <- ask_var_arena
    let SIR.Variable _ _ (Located _ name) = Arena.get var_arena var_key
    pure name

quant_vars_to_children ::
    Monad under =>
    [Type.QuantVarKey] ->
    NRReader adt_arena type_synonym_arena var_arena (Arena.Arena Type.QuantVar Type.QuantVarKey) sir_child_maps under [DeclChild]
quant_vars_to_children =
    mapM
        ( \var -> do
            quant_var_arena <- ask_quant_var_arena
            let (Type.QuantVar (Located name_sp name)) = Arena.get quant_var_arena var
            pure (name, DeclAt name_sp, SIR.DeclRef'Type $ TypeWithInferVar.Type'QuantVar var)
        )

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

-- intrinsics package child maps {{{1
-- TODO: not sure if this is the best place to put this
-- maybe just have a field in SIRChildMaps that represents the intrinsics package and then just populate it in the AssignNameMaps stage like
-- everything else? that seems odd because the intrinsics package always has the same hard-coded constants, but at least that would move this to the
-- right place (?)
intrinsics_package_child_maps :: ChildMaps
intrinsics_package_child_maps =
    ChildMaps $
        Maps
            Map.empty
            ( Map.fromList $
                map
                    (\intrinsic -> (Intrinsics.intrinsic_bv_name intrinsic, (ImplicitPrim, SIR.ValueRef'Intrinsic intrinsic)))
                    (Enum.enumFromTo Enum.minBound Enum.maxBound)
            )
            Map.empty
