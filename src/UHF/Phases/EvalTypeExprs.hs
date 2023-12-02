module UHF.Phases.EvalTypeExprs
    ( eval
    ) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Phases.NameResolve.Utils as Utils

-- TODO: change errors, clean up this whole module

type VIdenStart = Maybe SIR.BoundValueKey
type PIdenStart = Maybe Type.ADTVariantIndex

type EvaledDIden = Maybe SIR.DeclKey

type Unevaled = (EvaledDIden, (), (), VIdenStart, (), PIdenStart, (), (), ())

-- TODO: remove these type aliases
type UnevaledSIR = SIR.SIR Unevaled
type UnevaledModule = SIR.Module Unevaled
type UnevaledADT = Type.ADT (UnevaledTypeExpr, ())
type UnevaledTypeSynonym = Type.TypeSynonym (UnevaledTypeExpr, ())
type UnevaledTypeExpr = SIR.TypeExpr Unevaled
type UnevaledBinding = SIR.Binding Unevaled
type UnevaledExpr = SIR.Expr Unevaled
type UnevaledPattern = SIR.Pattern Unevaled

type UnevaledModuleArena = Arena.Arena UnevaledModule SIR.ModuleKey
type UnevaledADTArena = Arena.Arena UnevaledADT Type.ADTKey
type UnevaledTypeSynonymArena = Arena.Arena UnevaledTypeSynonym Type.TypeSynonymKey
type UnevaledBoundValueArena = Arena.Arena (SIR.BoundValue Unevaled) SIR.BoundValueKey

type Evaled = (EvaledDIden, EvaledDIden, Maybe (Type.Type Void), VIdenStart, (), PIdenStart, (), (), ())

type EvaledSIR = SIR.SIR Evaled
type EvaledModule = SIR.Module Evaled
type EvaledADT = Type.ADT (EvaledTypeExpr, Maybe (Type.Type Void))
type EvaledTypeSynonym = Type.TypeSynonym (EvaledTypeExpr, Maybe (Type.Type Void))
type EvaledTypeExpr = SIR.TypeExpr Evaled
type EvaledBinding = SIR.Binding Evaled
type EvaledExpr = SIR.Expr Evaled
type EvaledPattern = SIR.Pattern Evaled

type EvaledModuleArena = Arena.Arena EvaledModule SIR.ModuleKey
type EvaledADTArena = Arena.Arena EvaledADT Type.ADTKey
type EvaledTypeSynonymArena = Arena.Arena EvaledTypeSynonym Type.TypeSynonymKey

-- eval entry point {{{1
eval :: UnevaledSIR -> Compiler.WithDiagnostics Utils.Error Void EvaledSIR
eval (SIR.SIR decls mods adts type_synonyms type_vars bound_values mod) =
    runStateT
        (
            runReaderT (Utils.collect_child_maps mods type_synonyms) (adts, bound_values, (), ()) >>= \ module_child_maps ->
            runReaderT (resolve_in_mods mods) (adts, bound_values, (), module_child_maps) >>= \ mods ->
            runReaderT (resolve_in_adts adts) ((), (), (), module_child_maps) >>= \ adts ->
            runReaderT (resolve_in_type_synonyms type_synonyms) (decls, (), (), module_child_maps) >>= \ synonyms ->
            pure (mods, adts, synonyms)
        )
        decls >>= \ ((mods, adts, synonyms), decls) ->
    pure (SIR.SIR decls mods adts synonyms type_vars (Arena.transform change_bound_value bound_values) mod)
    where
        change_bound_value (SIR.BoundValue bvid tyinfo n) = SIR.BoundValue bvid tyinfo n
        change_bound_value (SIR.BoundValue'ADTVariant bvid id tyvars tyinfo sp) = SIR.BoundValue'ADTVariant bvid id tyvars tyinfo sp

-- resolving through sir {{{1
resolve_in_mods :: UnevaledModuleArena -> (Utils.NRReader UnevaledADTArena UnevaledBoundValueArena type_var_arena Utils.ModuleChildMaps (Utils.MakeDeclState Utils.CollectingErrors)) EvaledModuleArena
resolve_in_mods = Arena.transformM resolve_in_module

resolve_in_adts :: UnevaledADTArena -> (Utils.NRReader adt_arena bv_arena type_var_arena Utils.ModuleChildMaps (Utils.MakeDeclState Utils.CollectingErrors)) EvaledADTArena
resolve_in_adts = Arena.transformM resolve_in_adt

resolve_in_type_synonyms :: UnevaledTypeSynonymArena -> (Utils.NRReader adt_arena bv_arena type_var_arena Utils.ModuleChildMaps (Utils.MakeDeclState Utils.CollectingErrors)) EvaledTypeSynonymArena
resolve_in_type_synonyms = Arena.transformM resolve_in_type_synonym

resolve_in_module :: UnevaledModule -> Utils.NRReader UnevaledADTArena UnevaledBoundValueArena type_var_arena Utils.ModuleChildMaps (Utils.MakeDeclState Utils.CollectingErrors) EvaledModule
resolve_in_module (SIR.Module id bindings adts type_synonyms) = SIR.Module id <$> mapM resolve_in_binding bindings <*> pure adts <*> pure type_synonyms

resolve_in_adt :: UnevaledADT -> (Utils.NRReader adt_arena bv_arena type_var_arena Utils.ModuleChildMaps (Utils.MakeDeclState Utils.CollectingErrors)) EvaledADT
resolve_in_adt (Type.ADT id name type_vars variants) = Type.ADT id name type_vars <$> mapM resolve_in_variant variants
    where
        resolve_in_variant (Type.ADTVariant'Named name id fields) = Type.ADTVariant'Named name id <$> mapM (\ (id, name, (ty, ())) -> resolve_in_type_expr ty >>= \ ty -> lift (evaled_as_type ty) >>= \ ty_as_type -> pure (id, name, (ty, ty_as_type))) fields
        resolve_in_variant (Type.ADTVariant'Anon name id fields) = Type.ADTVariant'Anon name id <$> mapM (\ (id, (ty, ())) -> resolve_in_type_expr ty >>= \ ty -> lift (evaled_as_type ty) >>= \ ty_as_type -> pure (id, (ty, ty_as_type))) fields

resolve_in_type_synonym :: UnevaledTypeSynonym -> (Utils.NRReader adt_arena bv_arena type_var_arena Utils.ModuleChildMaps (Utils.MakeDeclState Utils.CollectingErrors)) EvaledTypeSynonym
resolve_in_type_synonym (Type.TypeSynonym id name (expansion, ())) =
    resolve_in_type_expr expansion >>= \ expansion ->
    lift (evaled_as_type expansion) >>= \ expansion_as_type ->
    pure (Type.TypeSynonym id name (expansion, expansion_as_type))

resolve_in_binding :: UnevaledBinding -> (Utils.NRReader UnevaledADTArena UnevaledBoundValueArena type_var_arena Utils.ModuleChildMaps (Utils.MakeDeclState Utils.CollectingErrors)) EvaledBinding
resolve_in_binding (SIR.Binding target eq_sp expr) = SIR.Binding <$> resolve_in_pat target <*> pure eq_sp <*> resolve_in_expr expr
resolve_in_binding (SIR.Binding'ADTVariant bvk variant vars sp) = pure $ SIR.Binding'ADTVariant bvk variant vars sp

-- TODO: all of the todos here will be fixed when types get rewritten
resolve_in_type_expr :: UnevaledTypeExpr -> (Utils.NRReader adt_arena bv_arena type_var_arena Utils.ModuleChildMaps (Utils.MakeDeclState Utils.CollectingErrors)) EvaledTypeExpr
resolve_in_type_expr (SIR.TypeExpr'Refer () sp iden) = pure (SIR.TypeExpr'Refer iden sp iden)
resolve_in_type_expr (SIR.TypeExpr'Get () sp parent name) = do
    decls <- lift Utils.ask_decl_arena
    mods <- Utils.ask_module_child_maps
    parent <- resolve_in_type_expr parent
    result <- case SIR.type_expr_evaled parent of
        Just parent -> case Utils.get_decl_child decls mods parent name of
            Right r -> pure $ Just r
            Left e -> lift (lift $ Compiler.tell_error e) >> pure Nothing
        Nothing -> pure Nothing

    pure (SIR.TypeExpr'Get result sp parent name)
resolve_in_type_expr (SIR.TypeExpr'Tuple () sp a b) =
    resolve_in_type_expr a >>= \ a_conv ->
    resolve_in_type_expr b >>= \ b_conv ->
    lift (evaled_as_type a_conv) >>= \ a_as_type ->
    lift (evaled_as_type b_conv) >>= \ b_as_type ->
    pure (SIR.TypeExpr'Tuple (todo $ Type.Type'Tuple <$> a_as_type <*> b_as_type) sp a_conv b_conv)
resolve_in_type_expr (SIR.TypeExpr'Hole () ty sp hid) = pure $ SIR.TypeExpr'Hole Nothing todo sp hid  -- TODO: also make this an unknown; also rewrite typing thingies
resolve_in_type_expr (SIR.TypeExpr'Function () sp arg res) =
    resolve_in_type_expr arg >>= \ arg ->
    resolve_in_type_expr res >>= \ res ->
    lift (evaled_as_type arg) >>= \ arg_as_type ->
    lift (evaled_as_type res) >>= \ res_as_type ->
    pure (SIR.TypeExpr'Function (todo $ Type.Type'Function <$> arg_as_type <*> res_as_type) sp arg res)
resolve_in_type_expr (SIR.TypeExpr'Forall () sp vars inner) =
    resolve_in_type_expr inner >>= \ inner ->
    lift (evaled_as_type inner) >>= \ inner_as_type ->
    pure (SIR.TypeExpr'Forall (todo (Type.Type'Forall vars <$> inner_as_type)) sp vars inner)
{- TODO
type_expr (SIR.TypeExpr'Apply () sp ty arg) =
    type_expr ty >>= \ ty ->
    type_expr arg >>= \ arg ->
    apply_type (TypeExpr sp) sp (_ ty) (_ arg) >>= \ result_ty ->
    pure (SIR.TypeExpr'Apply result_ty sp ty arg)
-}
resolve_in_type_expr (SIR.TypeExpr'Apply () sp ty args) = SIR.TypeExpr'Apply todo sp <$> resolve_in_type_expr ty <*> resolve_in_type_expr args
resolve_in_type_expr (SIR.TypeExpr'Wild () sp) = pure $ SIR.TypeExpr'Wild Nothing sp -- TODO: make this an unknown to be inferred and not a Nothing
resolve_in_type_expr (SIR.TypeExpr'Poison () sp) = pure $ SIR.TypeExpr'Poison Nothing sp

resolve_in_pat :: UnevaledPattern -> (Utils.NRReader adt_arena bv_arena type_var_arena Utils.ModuleChildMaps (Utils.MakeDeclState Utils.CollectingErrors)) EvaledPattern
resolve_in_pat (SIR.Pattern'Identifier type_info sp bnk) = pure $ SIR.Pattern'Identifier type_info sp bnk
resolve_in_pat (SIR.Pattern'Wildcard type_info sp) = pure $ SIR.Pattern'Wildcard type_info sp
resolve_in_pat (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp <$> resolve_in_pat a <*> resolve_in_pat b
resolve_in_pat (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk <$> resolve_in_pat subpat
resolve_in_pat (SIR.Pattern'AnonADTVariant type_info sp variant_split_iden () tyargs subpat) = SIR.Pattern'AnonADTVariant type_info sp <$> resolve_split_iden variant_split_iden <*> pure () <*> pure tyargs <*> mapM resolve_in_pat subpat
resolve_in_pat (SIR.Pattern'NamedADTVariant type_info sp variant_split_iden () tyargs subpat) = SIR.Pattern'NamedADTVariant type_info sp <$> resolve_split_iden variant_split_iden <*> pure () <*> pure tyargs <*> mapM (\ (field_name, field_pat) -> (field_name,) <$> resolve_in_pat field_pat) subpat
resolve_in_pat (SIR.Pattern'Poison type_info sp) = pure $ SIR.Pattern'Poison type_info sp

resolve_in_expr :: UnevaledExpr -> (Utils.NRReader UnevaledADTArena UnevaledBoundValueArena type_var_arena Utils.ModuleChildMaps (Utils.MakeDeclState Utils.CollectingErrors)) EvaledExpr
resolve_in_expr (SIR.Expr'Identifier id type_info sp iden_split ()) = SIR.Expr'Identifier id type_info sp <$> resolve_split_iden iden_split <*> pure ()
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
                (sp,,(),)
                    <$> resolve_split_iden iden
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

resolve_in_expr (SIR.Expr'TypeAnnotation id type_info sp (ty, ()) e) =
    resolve_in_type_expr ty >>= \ ty ->
    resolve_in_expr e >>= \ e ->
    lift (evaled_as_type ty) >>= \ ty_as_type ->
    pure (SIR.Expr'TypeAnnotation id type_info sp (ty, ty_as_type) e)

resolve_in_expr (SIR.Expr'Forall id type_info sp vars e) = SIR.Expr'Forall id type_info sp vars <$> resolve_in_expr e
resolve_in_expr (SIR.Expr'TypeApply id type_info sp e (arg, ())) = resolve_in_expr e >>= \ e -> resolve_in_type_expr arg >>= \ arg -> lift (evaled_as_type arg) >>= \ arg_as_type -> pure (SIR.Expr'TypeApply id type_info sp e (arg, arg_as_type))

resolve_in_expr (SIR.Expr'Hole id type_info sp hid) = pure $ SIR.Expr'Hole id type_info sp hid

resolve_in_expr (SIR.Expr'Poison id type_info sp) = pure $ SIR.Expr'Poison id type_info sp

-- resolving identifiers {{{1
resolve_split_iden :: SIR.SplitIdentifier Unevaled start -> Utils.NRReader adt_arena bv_arena type_var_arena Utils.ModuleChildMaps (Utils.MakeDeclState Utils.CollectingErrors) (SIR.SplitIdentifier Evaled start)
resolve_split_iden (SIR.SplitIdentifier'Get texpr next) = resolve_in_type_expr texpr >>= \ texpr -> pure (SIR.SplitIdentifier'Get texpr next)
resolve_split_iden (SIR.SplitIdentifier'Single start) = pure (SIR.SplitIdentifier'Single start)

evaled_as_type :: EvaledTypeExpr -> Utils.MakeDeclState Utils.CollectingErrors (Maybe (Type.Type Void))
evaled_as_type texpr =
    case SIR.type_expr_evaled texpr of
        Just evaled ->
            get >>= \ decls ->
            case Arena.get decls evaled of
                SIR.Decl'Module _ -> lift (Compiler.tell_error (Utils.Error'NotAType (SIR.type_expr_span texpr) "a module")) >> pure Nothing
                SIR.Decl'Type ty -> pure $ Just ty
        Nothing -> pure Nothing
