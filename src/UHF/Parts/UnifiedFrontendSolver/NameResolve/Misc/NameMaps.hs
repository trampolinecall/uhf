module UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NameMaps
    ( NameContextKey
    , NameContext (..)
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
    ) where

-- TODO: clean up this and the 3 modules too
-- TODO: clean up folds

import UHF.Prelude

import qualified Data.Map as Map

import qualified GHC.Enum as Enum (enumFromTo, maxBound, minBound)
import qualified UHF.Data.IR.Intrinsics as Intrinsics
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.IR.TypeWithInferVar as TypeWithInferVar
import qualified UHF.Data.SIR as SIR
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Error
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.DeclAt (DeclAt (..))
import UHF.Parts.UnifiedFrontendSolver.SolveResult
import UHF.Source.Located (Located (unlocate))
import qualified UHF.Util.Arena as Arena
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Refs (DeclRef (..), ValueRef (..))

-- ChildMaps and NameMaps {{{1
-- Maps is so that the functions that act on ChildMaps and NameMaps aren't so duplicated: by having ChildMaps and NameMaps be newtypes of Maps, we
-- can have 1 function that does some operation on Maps and then 2 exported functions that unwraps the ChildMaps and NameMaps newtypes and applies
-- that 1 function to the underlying Maps instead of having 2 almost identical functions for both ChildMaps and NameMaps
data Maps
    = Maps (Map.Map Text (DeclAt, DeclRef TypeWithInferVar.Type)) (Map.Map Text (DeclAt, ValueRef)) (Map.Map Text (DeclAt, Type.ADT.VariantIndex))
    deriving Show

-- ChilsMaps and NameMaps are very similar datatypes. the difference between them is conceptual: ChildMaps is a map that tells what the children of a
-- certain entity are, whereas a NameMap just stores what names are currently in scope (and is only used for resolving roots). in some cases the
-- distinction is actually important. consider for example an ADT:
--     data X #A #B #C {
--         ...
--     }
-- A, B, and C are all in scope inside of X so that variants and declarations inside X can use them, so they are part of the NameMap that represents
-- the names in scope inside of X, but A, B, and C are not part of the child maps of X, so you can't refer to X::A.
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
type DeclChild = Child (DeclRef TypeWithInferVar.Type)
type ValueChild = Child ValueRef
type ADTVariantChild = Child Type.ADT.VariantIndex

add_to_maps :: [DeclChild] -> [ValueChild] -> [ADTVariantChild] -> Maps -> Writer [Error] Maps
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

        add :: Map Text (DeclAt, thing) -> Child thing -> Writer [Error] (Map Text (DeclAt, thing))
        add map (name, decl_at, resolved) =
            case Map.lookup name map of
                Just (prev_decl_at, _) -> do
                    _ <- tell [Error'DuplicateDecl name prev_decl_at decl_at]
                    pure map
                Nothing -> pure $ Map.insert name (decl_at, resolved) map

add_to_child_maps :: [DeclChild] -> [ValueChild] -> [ADTVariantChild] -> ChildMaps -> (ChildMaps, [Error])
add_to_child_maps ds vs as (ChildMaps m) = runWriter $ ChildMaps <$> add_to_maps ds vs as m
add_to_name_maps :: [DeclChild] -> [ValueChild] -> [ADTVariantChild] -> NameMaps -> (NameMaps, [Error])
add_to_name_maps ds vs as (NameMaps m) = runWriter $ NameMaps <$> add_to_maps ds vs as m

add_tuple_to_child_maps :: ([DeclChild], [ValueChild], [ADTVariantChild]) -> ChildMaps -> (ChildMaps, [Error])
add_tuple_to_child_maps (ds, vs, as) (ChildMaps m) = runWriter $ ChildMaps <$> add_to_maps ds vs as m
add_tuple_to_name_maps :: ([DeclChild], [ValueChild], [ADTVariantChild]) -> NameMaps -> (NameMaps, [Error])
add_tuple_to_name_maps (ds, vs, as) (NameMaps m) = runWriter $ NameMaps <$> add_to_maps ds vs as m

-- NameContext {{{1
data NameContext = NameContext NameMaps (Maybe NameContextKey)
newtype NameContextKey = NameContextKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key NameContextKey where
    make_key = NameContextKey
    unmake_key (NameContextKey i) = i

-- SIRChildMaps {{{1
newtype SIRChildMaps = SIRChildMaps (Arena.Arena ChildMaps SIR.ModuleKey)

empty_sir_child_maps :: SIR.SIR stage -> SIRChildMaps
empty_sir_child_maps (SIR.SIR mod_arena _ _ _ _ _) = SIRChildMaps $ Arena.transform go mod_arena
    where
        go (SIR.Module _ _ _ _ _ _) = empty_child_maps

get_module_child_maps :: SIRChildMaps -> SIR.ModuleKey -> ChildMaps
get_module_child_maps (SIRChildMaps module_child_maps) = Arena.get module_child_maps

add_to_module_child_maps :: [DeclChild] -> [ValueChild] -> [ADTVariantChild] -> SIR.ModuleKey -> SIRChildMaps -> (SIRChildMaps, [Error])
add_to_module_child_maps ds vs as mod_key (SIRChildMaps module_child_maps) = runWriter $ SIRChildMaps <$> Arena.modifyM module_child_maps mod_key (writer . add_to_child_maps ds vs as) -- TODO: this ad hoc writer runWriter is really weird and should probably be refactored to something else (incidentally, it will probably just compile out and be fine)

add_tuple_to_module_child_maps :: ([DeclChild], [ValueChild], [ADTVariantChild]) -> SIR.ModuleKey -> SIRChildMaps -> (SIRChildMaps, [Error])
add_tuple_to_module_child_maps (ds, vs, as) mod_key (SIRChildMaps module_child_maps) = runWriter $ SIRChildMaps <$> Arena.modifyM module_child_maps mod_key (writer . add_to_child_maps ds vs as)

-- getting from name maps and child maps {{{1
look_up_decl ::
    Arena.Arena NameContext NameContextKey -> NameContextKey -> Located Text -> SolveResult Error Error (DeclRef TypeWithInferVar.Type)
look_up_decl = look_up (\(NameMaps (Maps d _ _)) -> d)
look_up_value :: Arena.Arena NameContext NameContextKey -> NameContextKey -> Located Text -> SolveResult Error Error ValueRef
look_up_value = look_up (\(NameMaps (Maps _ val _)) -> val)
look_up_variant :: Arena.Arena NameContext NameContextKey -> NameContextKey -> Located Text -> SolveResult Error Error Type.ADT.VariantIndex
look_up_variant = look_up (\(NameMaps (Maps _ _ var)) -> var)

look_up ::
    (NameMaps -> Map Text (DeclAt, result)) ->
    Arena.Arena NameContext NameContextKey ->
    NameContextKey ->
    Located Text ->
    SolveResult Error Error result
look_up which_map arena name_map_stack iden =
    go name_map_stack
    where
        go current_name_map_stack_key =
            let NameContext current_name_maps parent = Arena.get arena current_name_map_stack_key
            in case Map.lookup (unlocate iden) (which_map current_name_maps) of
                Just (_, decl) -> Solved decl
                Nothing ->
                    case parent of
                        Just parent -> go parent
                        Nothing -> Errored $ Error'CouldNotFind iden

get_decl_child :: SIRChildMaps -> DeclRef TypeWithInferVar.Type -> Located Text -> SolveResult Error Error (DeclRef TypeWithInferVar.Type)
get_decl_child sir_child_maps decl name =
    let res = case decl of
            DeclRef'Module m ->
                let ChildMaps (Maps d_children _ _) = get_module_child_maps sir_child_maps m
                in Map.lookup (unlocate name) d_children
            DeclRef'Type _ -> Nothing
            DeclRef'ExternPackage SIR.ExternPackage'IntrinsicsPackage ->
                let ChildMaps (Maps d_children _ _) = intrinsics_package_child_maps
                in Map.lookup (unlocate name) d_children
    in case res of
        Just (_, res) -> Solved res
        Nothing -> Errored $ Error'CouldNotFindIn Nothing name -- TODO: put previous
get_value_child :: SIRChildMaps -> DeclRef TypeWithInferVar.Type -> Located Text -> SolveResult Error Error ValueRef
get_value_child sir_child_maps decl name =
    let res = case decl of
            DeclRef'Module m ->
                let ChildMaps (Maps _ v_children _) = get_module_child_maps sir_child_maps m
                in Map.lookup (unlocate name) v_children
            DeclRef'Type _ -> Nothing
            DeclRef'ExternPackage SIR.ExternPackage'IntrinsicsPackage ->
                let ChildMaps (Maps _ v_children _) = intrinsics_package_child_maps
                in Map.lookup (unlocate name) v_children
    in case res of
        Just (_, res) -> Solved res
        Nothing -> Errored $ Error'CouldNotFindIn Nothing name -- TODO: put previous
get_variant_child :: SIRChildMaps -> DeclRef TypeWithInferVar.Type -> Located Text -> SolveResult Error Error Type.ADT.VariantIndex
get_variant_child sir_child_maps decl name =
    let res = case decl of
            DeclRef'Module m ->
                let ChildMaps (Maps _ _ adtv_children) = get_module_child_maps sir_child_maps m
                in Map.lookup (unlocate name) adtv_children
            DeclRef'Type _ -> Nothing
            DeclRef'ExternPackage SIR.ExternPackage'IntrinsicsPackage ->
                let ChildMaps (Maps _ _ adtv_children) = intrinsics_package_child_maps
                in Map.lookup (unlocate name) adtv_children
    in case res of
        Just (_, res) -> Solved res
        Nothing -> Errored $ Error'CouldNotFindIn Nothing name -- TODO: put previous

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
                    (\intrinsic -> (Intrinsics.intrinsic_name intrinsic, (ImplicitPrim, ValueRef'Intrinsic intrinsic)))
                    (Enum.enumFromTo Enum.minBound Enum.maxBound)
            )
            Map.empty
