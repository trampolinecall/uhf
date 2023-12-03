module UHF.Phases.NameResolve.ResolveVPIdens
    ( resolve
    , Unresolved
    , Resolved
    ) where

import UHF.Prelude

import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.TypeSolver as TypeSolver
import qualified UHF.Phases.NameResolve.Error as Error
import qualified UHF.Phases.NameResolve.NRReader as NRReader
import qualified UHF.Phases.NameResolve.NameMaps as NameMaps
import qualified UHF.Util.Arena as Arena

type DIden = Maybe (SIR.Decl TypeSolver.Type)

type UnresolvedVIden = SIR.SplitIdentifier Unresolved ResolvedVIden
type UnresolvedPIden = SIR.SplitIdentifier Unresolved ResolvedPIden

type ResolvedVIden = Maybe SIR.BoundValue
type ResolvedPIden = Maybe Type.ADT.VariantIndex

type Unresolved = (DIden, DIden, TypeSolver.Type, ResolvedVIden, (), ResolvedPIden, (), (), ())

type UnresolvedADT = Type.ADT (SIR.TypeExpr Unresolved, TypeSolver.Type)
type UnresolvedTypeSynonym = Type.TypeSynonym (SIR.TypeExpr Unresolved, TypeSolver.Type)

type UnresolvedModuleArena = Arena.Arena (SIR.Module Unresolved) SIR.ModuleKey
type UnresolvedADTArena = Arena.Arena UnresolvedADT Type.ADTKey
type UnresolvedTypeSynonymArena = Arena.Arena UnresolvedTypeSynonym Type.TypeSynonymKey
type UnresolvedVariableArena = Arena.Arena (SIR.Variable Unresolved) SIR.VariableKey

type Resolved = (DIden, DIden, TypeSolver.Type, ResolvedVIden, ResolvedVIden, ResolvedPIden, ResolvedPIden, (), ())

type ResolvedADT = Type.ADT (SIR.TypeExpr Resolved, TypeSolver.Type)
type ResolvedTypeSynonym = Type.TypeSynonym (SIR.TypeExpr Resolved, TypeSolver.Type)

type ResolvedModuleArena = Arena.Arena (SIR.Module Resolved) SIR.ModuleKey
type ResolvedADTArena = Arena.Arena ResolvedADT Type.ADTKey
type ResolvedTypeSynonymArena = Arena.Arena ResolvedTypeSynonym Type.TypeSynonymKey

-- resolve entry point {{{1
resolve :: NameMaps.SIRChildMaps -> SIR.SIR Unresolved -> Error.WithErrors (SIR.SIR Resolved)
resolve sir_child_maps (SIR.SIR mods adts type_synonyms type_vars variables mod) =
    runReaderT (resolve_in_mods mods) (adts, variables, (), sir_child_maps) >>= \ mods ->
    runReaderT (resolve_in_adts adts) ((), (), (), sir_child_maps) >>= \ adts ->
    runReaderT (resolve_in_type_synonyms type_synonyms) ((), (), (), sir_child_maps) >>= \ synonyms ->
    pure (SIR.SIR mods adts synonyms type_vars (Arena.transform change_variable variables) mod)
    where
        change_variable (SIR.Variable varid tyinfo n) = SIR.Variable varid tyinfo n

-- resolving through sir {{{1
resolve_in_mods :: UnresolvedModuleArena -> (NRReader.NRReader UnresolvedADTArena UnresolvedVariableArena type_var_arena NameMaps.SIRChildMaps Error.WithErrors) ResolvedModuleArena
resolve_in_mods = Arena.transformM resolve_in_module

resolve_in_adts :: UnresolvedADTArena -> (NRReader.NRReader adt_arena var_arena type_var_arena NameMaps.SIRChildMaps Error.WithErrors) ResolvedADTArena
resolve_in_adts = Arena.transformM resolve_in_adt

resolve_in_type_synonyms :: UnresolvedTypeSynonymArena -> (NRReader.NRReader adt_arena var_arena type_var_arena NameMaps.SIRChildMaps Error.WithErrors) ResolvedTypeSynonymArena
resolve_in_type_synonyms = Arena.transformM resolve_in_type_synonym

resolve_in_module :: SIR.Module Unresolved -> NRReader.NRReader UnresolvedADTArena UnresolvedVariableArena type_var_arena NameMaps.SIRChildMaps Error.WithErrors (SIR.Module Resolved)
resolve_in_module (SIR.Module id bindings adts type_synonyms) = SIR.Module id <$> mapM resolve_in_binding bindings <*> pure adts <*> pure type_synonyms

resolve_in_adt :: UnresolvedADT -> (NRReader.NRReader adt_arena var_arena type_var_arena NameMaps.SIRChildMaps Error.WithErrors) ResolvedADT
resolve_in_adt (Type.ADT id name type_vars variants) = Type.ADT id name type_vars <$> mapM resolve_in_variant variants
    where
        resolve_in_variant (Type.ADT.Variant'Named name id fields) = Type.ADT.Variant'Named name id <$> mapM (\ (id, name, (ty, teat)) -> resolve_in_type_expr ty >>= \ ty -> pure (id, name, (ty, teat))) fields -- 'teat' short for 'type evaluated as type'
        resolve_in_variant (Type.ADT.Variant'Anon name id fields) = Type.ADT.Variant'Anon name id <$> mapM (\ (id, (ty, teat)) -> resolve_in_type_expr ty >>= \ ty -> pure (id, (ty, teat))) fields

resolve_in_type_synonym :: UnresolvedTypeSynonym -> (NRReader.NRReader adt_arena var_arena type_var_arena NameMaps.SIRChildMaps Error.WithErrors) ResolvedTypeSynonym
resolve_in_type_synonym (Type.TypeSynonym id name (expansion, expeat)) = resolve_in_type_expr expansion >>= \ expansion -> pure (Type.TypeSynonym id name (expansion, expeat))

resolve_in_binding :: SIR.Binding Unresolved -> (NRReader.NRReader UnresolvedADTArena UnresolvedVariableArena type_var_arena NameMaps.SIRChildMaps Error.WithErrors) (SIR.Binding Resolved)
resolve_in_binding (SIR.Binding target eq_sp expr) = SIR.Binding <$> resolve_in_pat target <*> pure eq_sp <*> resolve_in_expr expr

resolve_in_type_expr :: SIR.TypeExpr Unresolved -> (NRReader.NRReader adt_arena var_arena type_var_arena NameMaps.SIRChildMaps Error.WithErrors) (SIR.TypeExpr Resolved)
resolve_in_type_expr (SIR.TypeExpr'Refer evaled sp id) = pure $ SIR.TypeExpr'Refer evaled sp id
resolve_in_type_expr (SIR.TypeExpr'Get evaled sp parent name) = SIR.TypeExpr'Get evaled sp <$> resolve_in_type_expr parent <*> pure name
resolve_in_type_expr (SIR.TypeExpr'Tuple evaled sp a b) = SIR.TypeExpr'Tuple evaled sp <$> resolve_in_type_expr a <*> resolve_in_type_expr b
resolve_in_type_expr (SIR.TypeExpr'Hole evaled type_info sp hid) = pure $ SIR.TypeExpr'Hole evaled type_info sp hid
resolve_in_type_expr (SIR.TypeExpr'Function evaled sp arg res) = SIR.TypeExpr'Function evaled sp <$> resolve_in_type_expr arg <*> resolve_in_type_expr res
resolve_in_type_expr (SIR.TypeExpr'Forall evaled sp vars ty) = SIR.TypeExpr'Forall evaled sp vars <$> resolve_in_type_expr ty
resolve_in_type_expr (SIR.TypeExpr'Apply evaled sp ty args) = SIR.TypeExpr'Apply evaled sp <$> resolve_in_type_expr ty <*> resolve_in_type_expr args
resolve_in_type_expr (SIR.TypeExpr'Wild evaled sp) = pure $ SIR.TypeExpr'Wild evaled sp
resolve_in_type_expr (SIR.TypeExpr'Poison evaled sp) = pure $ SIR.TypeExpr'Poison evaled sp

resolve_in_pat :: SIR.Pattern Unresolved -> (NRReader.NRReader adt_arena var_arena type_var_arena NameMaps.SIRChildMaps Error.WithErrors) (SIR.Pattern Resolved)
resolve_in_pat (SIR.Pattern'Identifier type_info sp bnk) = pure $ SIR.Pattern'Identifier type_info sp bnk
resolve_in_pat (SIR.Pattern'Wildcard type_info sp) = pure $ SIR.Pattern'Wildcard type_info sp
resolve_in_pat (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp <$> resolve_in_pat a <*> resolve_in_pat b
resolve_in_pat (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk <$> resolve_in_pat subpat
resolve_in_pat (SIR.Pattern'AnonADTVariant type_info sp variant_iden () tyargs subpat) = SIR.Pattern'AnonADTVariant type_info sp <$> resolve_split_iden variant_iden <*> resolve_iden_in_monad resolve_pat_iden variant_iden <*> pure tyargs <*> mapM resolve_in_pat subpat
resolve_in_pat (SIR.Pattern'NamedADTVariant type_info sp variant_iden () tyargs subpat) = SIR.Pattern'NamedADTVariant type_info sp <$> resolve_split_iden variant_iden <*> resolve_iden_in_monad resolve_pat_iden variant_iden <*> pure tyargs <*> mapM (\ (field_name, field_pat) -> (field_name,) <$> resolve_in_pat field_pat) subpat
resolve_in_pat (SIR.Pattern'Poison type_info sp) = pure $ SIR.Pattern'Poison type_info sp

resolve_in_expr :: SIR.Expr Unresolved -> (NRReader.NRReader UnresolvedADTArena UnresolvedVariableArena type_var_arena NameMaps.SIRChildMaps Error.WithErrors) (SIR.Expr Resolved)
resolve_in_expr (SIR.Expr'Identifier id type_info sp split_iden ()) = SIR.Expr'Identifier id type_info sp <$> resolve_split_iden split_iden <*> resolve_iden_in_monad resolve_expr_iden split_iden
resolve_in_expr (SIR.Expr'Char id type_info sp c) = pure $ SIR.Expr'Char id type_info sp c
resolve_in_expr (SIR.Expr'String id type_info sp s) = pure $ SIR.Expr'String id type_info sp s
resolve_in_expr (SIR.Expr'Int id type_info sp i) = pure $ SIR.Expr'Int id type_info sp i
resolve_in_expr (SIR.Expr'Float id type_info sp f) = pure $ SIR.Expr'Float id type_info sp f
resolve_in_expr (SIR.Expr'Bool id type_info sp b) = pure $ SIR.Expr'Bool id type_info sp b

resolve_in_expr (SIR.Expr'Tuple id type_info sp a b) = SIR.Expr'Tuple id type_info sp <$> resolve_in_expr a <*> resolve_in_expr b

resolve_in_expr (SIR.Expr'Lambda id type_info sp param body) =
    SIR.Expr'Lambda id type_info sp <$> resolve_in_pat param <*> resolve_in_expr body

resolve_in_expr (SIR.Expr'Let id type_info sp bindings body) =
    SIR.Expr'Let id type_info sp <$> mapM resolve_in_binding bindings <*> resolve_in_expr body

resolve_in_expr (SIR.Expr'LetRec id type_info sp bindings body) =
    SIR.Expr'LetRec id type_info sp <$> mapM resolve_in_binding bindings <*> resolve_in_expr body

resolve_in_expr (SIR.Expr'BinaryOps id allowed type_info sp first ops) =
    SIR.Expr'BinaryOps id allowed type_info sp
        <$> resolve_in_expr first
        <*> mapM
            (\ (sp, iden, (), rhs) ->
                (sp,,,)
                    <$> resolve_split_iden iden
                    <*> resolve_iden_in_monad resolve_expr_iden iden
                    <*> resolve_in_expr rhs)
            ops

resolve_in_expr (SIR.Expr'Call id type_info sp callee arg) = SIR.Expr'Call id type_info sp <$> resolve_in_expr callee <*> resolve_in_expr arg

resolve_in_expr (SIR.Expr'If id type_info sp if_sp cond t f) = SIR.Expr'If id type_info sp if_sp <$> resolve_in_expr cond <*> resolve_in_expr t <*> resolve_in_expr f
resolve_in_expr (SIR.Expr'Match id type_info sp match_tok_sp e arms) =
    SIR.Expr'Match id type_info sp match_tok_sp
        <$> resolve_in_expr e
        <*> mapM
                (\ (pat, expr) ->
                    (,)
                        <$> resolve_in_pat pat
                        <*> resolve_in_expr expr
                )
                arms

resolve_in_expr (SIR.Expr'TypeAnnotation id type_info sp (ty, tye_ty) e) = SIR.Expr'TypeAnnotation id type_info sp <$> ((,tye_ty) <$> resolve_in_type_expr ty) <*> resolve_in_expr e

resolve_in_expr (SIR.Expr'Forall id type_info sp vars e) = SIR.Expr'Forall id type_info sp vars <$> resolve_in_expr e
resolve_in_expr (SIR.Expr'TypeApply id type_info sp e (arg, arg_ty)) = SIR.Expr'TypeApply id type_info sp <$> resolve_in_expr e <*> ((, arg_ty) <$> resolve_in_type_expr arg)

resolve_in_expr (SIR.Expr'Hole id type_info sp hid) = pure $ SIR.Expr'Hole id type_info sp hid

resolve_in_expr (SIR.Expr'Poison id type_info sp) = pure $ SIR.Expr'Poison id type_info sp

-- resolving identifiers {{{1
resolve_split_iden :: SIR.SplitIdentifier Unresolved start -> NRReader.NRReader adt_arena var_arena type_var_arena NameMaps.SIRChildMaps Error.WithErrors (SIR.SplitIdentifier Resolved start)
resolve_split_iden (SIR.SplitIdentifier'Get texpr next) = resolve_in_type_expr texpr >>= \ texpr -> pure (SIR.SplitIdentifier'Get texpr next)
resolve_split_iden (SIR.SplitIdentifier'Single start) = pure (SIR.SplitIdentifier'Single start)

-- TODO: remove this?
resolve_iden_in_monad :: Monad under => (NameMaps.SIRChildMaps -> iden -> under resolved) -> iden -> NRReader.NRReader adt_arena var_arena type_var_arena NameMaps.SIRChildMaps under resolved
resolve_iden_in_monad f iden =
    NRReader.ask_sir_child_maps >>= \ sir_child_maps ->
    lift (f sir_child_maps iden)

-- TODO: factor out repeated code?
resolve_expr_iden :: NameMaps.SIRChildMaps -> UnresolvedVIden -> Error.WithErrors ResolvedVIden
resolve_expr_iden sir_child_maps (SIR.SplitIdentifier'Get type_part name) =
    case NameMaps.get_value_child sir_child_maps <$> SIR.type_expr_evaled type_part <*> pure name of
        Just (Right v) -> pure $ Just v
        Just (Left e) -> Compiler.tell_error e >> pure Nothing
        Nothing -> pure Nothing
resolve_expr_iden _ (SIR.SplitIdentifier'Single iden) = pure iden

resolve_pat_iden :: NameMaps.SIRChildMaps -> UnresolvedPIden -> Error.WithErrors ResolvedPIden
resolve_pat_iden sir_child_maps (SIR.SplitIdentifier'Get type_part name) =
    case NameMaps.get_variant_child sir_child_maps <$> SIR.type_expr_evaled type_part <*> pure name of
        Just (Right v) -> pure $ Just v
        Just (Left e) -> Compiler.tell_error e >> pure Nothing
        Nothing -> pure Nothing
resolve_pat_iden _ (SIR.SplitIdentifier'Single iden) = pure iden
