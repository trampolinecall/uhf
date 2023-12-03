module UHF.Phases.NameResolve.EvalTypeExprs
    ( eval
    , Unevaled
    , Evaled
    ) where

import UHF.Prelude

import qualified UHF.Util.Arena as Arena
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.SIR as SIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Phases.NameResolve.Utils as Utils

-- TODO: change errors, clean up this whole module

type VIdenStart = Maybe SIR.VariableKey
type PIdenStart = Maybe Type.ADTVariantIndex

type EvaledDIden = Maybe SIR.Decl

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
type UnevaledVariableArena = Arena.Arena (SIR.Variable Unevaled) SIR.VariableKey

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
eval :: Utils.SIRChildMaps -> UnevaledSIR -> Compiler.WithDiagnostics Utils.Error Void EvaledSIR
eval sir_child_maps (SIR.SIR mods adts type_synonyms type_vars variables mod) =
    runReaderT (eval_in_mods mods) (adts, variables, (), sir_child_maps) >>= \ mods ->
    runReaderT (eval_in_adts adts) ((), (), (), sir_child_maps) >>= \ adts ->
    runReaderT (eval_in_type_synonyms type_synonyms) ((), (), (), sir_child_maps) >>= \ synonyms ->
    pure (SIR.SIR mods adts synonyms type_vars (Arena.transform change_variable variables) mod)
    where
        change_variable (SIR.Variable varid tyinfo n) = SIR.Variable varid tyinfo n
        change_variable (SIR.Variable'ADTVariant varid id tyvars tyinfo sp) = SIR.Variable'ADTVariant varid id tyvars tyinfo sp

-- resolving through sir {{{1
eval_in_mods :: UnevaledModuleArena -> (Utils.NRReader UnevaledADTArena UnevaledVariableArena type_var_arena Utils.SIRChildMaps Utils.WithErrors) EvaledModuleArena
eval_in_mods = Arena.transformM eval_in_module

eval_in_adts :: UnevaledADTArena -> (Utils.NRReader adt_arena var_arena type_var_arena Utils.SIRChildMaps Utils.WithErrors) EvaledADTArena
eval_in_adts = Arena.transformM eval_in_adt

eval_in_type_synonyms :: UnevaledTypeSynonymArena -> (Utils.NRReader adt_arena var_arena type_var_arena Utils.SIRChildMaps Utils.WithErrors) EvaledTypeSynonymArena
eval_in_type_synonyms = Arena.transformM eval_in_type_synonym

eval_in_module :: UnevaledModule -> Utils.NRReader UnevaledADTArena UnevaledVariableArena type_var_arena Utils.SIRChildMaps Utils.WithErrors EvaledModule
eval_in_module (SIR.Module id bindings adts type_synonyms) = SIR.Module id <$> mapM eval_in_binding bindings <*> pure adts <*> pure type_synonyms

eval_in_adt :: UnevaledADT -> (Utils.NRReader adt_arena var_arena type_var_arena Utils.SIRChildMaps Utils.WithErrors) EvaledADT
eval_in_adt (Type.ADT id name type_vars variants) = Type.ADT id name type_vars <$> mapM eval_in_variant variants
    where
        eval_in_variant (Type.ADTVariant'Named name id fields) = Type.ADTVariant'Named name id <$> mapM (\ (id, name, (ty, ())) -> eval_in_type_expr ty >>= \ ty -> lift (evaled_as_type ty) >>= \ ty_as_type -> pure (id, name, (ty, ty_as_type))) fields
        eval_in_variant (Type.ADTVariant'Anon name id fields) = Type.ADTVariant'Anon name id <$> mapM (\ (id, (ty, ())) -> eval_in_type_expr ty >>= \ ty -> lift (evaled_as_type ty) >>= \ ty_as_type -> pure (id, (ty, ty_as_type))) fields

eval_in_type_synonym :: UnevaledTypeSynonym -> (Utils.NRReader adt_arena var_arena type_var_arena Utils.SIRChildMaps Utils.WithErrors) EvaledTypeSynonym
eval_in_type_synonym (Type.TypeSynonym id name (expansion, ())) =
    eval_in_type_expr expansion >>= \ expansion ->
    lift (evaled_as_type expansion) >>= \ expansion_as_type ->
    pure (Type.TypeSynonym id name (expansion, expansion_as_type))

eval_in_binding :: UnevaledBinding -> (Utils.NRReader UnevaledADTArena UnevaledVariableArena type_var_arena Utils.SIRChildMaps Utils.WithErrors) EvaledBinding
eval_in_binding (SIR.Binding target eq_sp expr) = SIR.Binding <$> eval_in_pat target <*> pure eq_sp <*> eval_in_expr expr
eval_in_binding (SIR.Binding'ADTVariant var_key variant vars sp) = pure $ SIR.Binding'ADTVariant var_key variant vars sp

-- TODO: all of the todos here will be fixed when types get rewritten
eval_in_type_expr :: UnevaledTypeExpr -> (Utils.NRReader adt_arena var_arena type_var_arena Utils.SIRChildMaps Utils.WithErrors) EvaledTypeExpr
eval_in_type_expr (SIR.TypeExpr'Refer () sp iden) = pure (SIR.TypeExpr'Refer iden sp iden)
eval_in_type_expr (SIR.TypeExpr'Get () sp parent name) = do
    sir_child_maps <- Utils.ask_sir_child_maps
    parent <- eval_in_type_expr parent
    result <- case SIR.type_expr_evaled parent of
        Just parent -> case Utils.get_decl_child sir_child_maps parent name of
            Right r -> pure $ Just r
            Left e -> lift (Compiler.tell_error e) >> pure Nothing
        Nothing -> pure Nothing

    pure (SIR.TypeExpr'Get result sp parent name)
eval_in_type_expr (SIR.TypeExpr'Tuple () sp a b) =
    eval_in_type_expr a >>= \ a_conv ->
    eval_in_type_expr b >>= \ b_conv ->
    lift (evaled_as_type a_conv) >>= \ a_as_type ->
    lift (evaled_as_type b_conv) >>= \ b_as_type ->
    pure (SIR.TypeExpr'Tuple (todo $ Type.Type'Tuple <$> a_as_type <*> b_as_type) sp a_conv b_conv)
eval_in_type_expr (SIR.TypeExpr'Hole () ty sp hid) = pure $ SIR.TypeExpr'Hole Nothing todo sp hid  -- TODO: also make this an unknown; also rewrite typing thingies
eval_in_type_expr (SIR.TypeExpr'Function () sp arg res) =
    eval_in_type_expr arg >>= \ arg ->
    eval_in_type_expr res >>= \ res ->
    lift (evaled_as_type arg) >>= \ arg_as_type ->
    lift (evaled_as_type res) >>= \ res_as_type ->
    pure (SIR.TypeExpr'Function (todo $ Type.Type'Function <$> arg_as_type <*> res_as_type) sp arg res)
eval_in_type_expr (SIR.TypeExpr'Forall () sp vars inner) =
    eval_in_type_expr inner >>= \ inner ->
    lift (evaled_as_type inner) >>= \ inner_as_type ->
    pure (SIR.TypeExpr'Forall (todo (Type.Type'Forall vars <$> inner_as_type)) sp vars inner)
{- TODO
type_expr (SIR.TypeExpr'Apply () sp ty arg) =
    type_expr ty >>= \ ty ->
    type_expr arg >>= \ arg ->
    apply_type (TypeExpr sp) sp (_ ty) (_ arg) >>= \ result_ty ->
    pure (SIR.TypeExpr'Apply result_ty sp ty arg)
-}
eval_in_type_expr (SIR.TypeExpr'Apply () sp ty args) = SIR.TypeExpr'Apply todo sp <$> eval_in_type_expr ty <*> eval_in_type_expr args
eval_in_type_expr (SIR.TypeExpr'Wild () sp) = pure $ SIR.TypeExpr'Wild Nothing sp -- TODO: make this an unknown to be inferred and not a Nothing
eval_in_type_expr (SIR.TypeExpr'Poison () sp) = pure $ SIR.TypeExpr'Poison Nothing sp

eval_in_pat :: UnevaledPattern -> (Utils.NRReader adt_arena var_arena type_var_arena Utils.SIRChildMaps Utils.WithErrors) EvaledPattern
eval_in_pat (SIR.Pattern'Identifier type_info sp bnk) = pure $ SIR.Pattern'Identifier type_info sp bnk
eval_in_pat (SIR.Pattern'Wildcard type_info sp) = pure $ SIR.Pattern'Wildcard type_info sp
eval_in_pat (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp <$> eval_in_pat a <*> eval_in_pat b
eval_in_pat (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk <$> eval_in_pat subpat
eval_in_pat (SIR.Pattern'AnonADTVariant type_info sp variant_split_iden () tyargs subpat) = SIR.Pattern'AnonADTVariant type_info sp <$> eval_split_iden variant_split_iden <*> pure () <*> pure tyargs <*> mapM eval_in_pat subpat
eval_in_pat (SIR.Pattern'NamedADTVariant type_info sp variant_split_iden () tyargs subpat) = SIR.Pattern'NamedADTVariant type_info sp <$> eval_split_iden variant_split_iden <*> pure () <*> pure tyargs <*> mapM (\ (field_name, field_pat) -> (field_name,) <$> eval_in_pat field_pat) subpat
eval_in_pat (SIR.Pattern'Poison type_info sp) = pure $ SIR.Pattern'Poison type_info sp

eval_in_expr :: UnevaledExpr -> (Utils.NRReader UnevaledADTArena UnevaledVariableArena type_var_arena Utils.SIRChildMaps Utils.WithErrors) EvaledExpr
eval_in_expr (SIR.Expr'Identifier id type_info sp iden_split ()) = SIR.Expr'Identifier id type_info sp <$> eval_split_iden iden_split <*> pure ()
eval_in_expr (SIR.Expr'Char id type_info sp c) = pure $ SIR.Expr'Char id type_info sp c
eval_in_expr (SIR.Expr'String id type_info sp s) = pure $ SIR.Expr'String id type_info sp s
eval_in_expr (SIR.Expr'Int id type_info sp i) = pure $ SIR.Expr'Int id type_info sp i
eval_in_expr (SIR.Expr'Float id type_info sp f) = pure $ SIR.Expr'Float id type_info sp f
eval_in_expr (SIR.Expr'Bool id type_info sp b) = pure $ SIR.Expr'Bool id type_info sp b

eval_in_expr (SIR.Expr'Tuple id type_info sp a b) = SIR.Expr'Tuple id type_info sp <$> eval_in_expr a <*> eval_in_expr b

eval_in_expr (SIR.Expr'Lambda id type_info sp param body) =
    SIR.Expr'Lambda id type_info sp <$> eval_in_pat param <*> eval_in_expr body

eval_in_expr (SIR.Expr'Let id type_info sp bindings body) = SIR.Expr'Let id type_info sp <$> mapM eval_in_binding bindings <*> eval_in_expr body
eval_in_expr (SIR.Expr'LetRec id type_info sp bindings body) = SIR.Expr'LetRec id type_info sp <$> mapM eval_in_binding bindings <*> eval_in_expr body

eval_in_expr (SIR.Expr'BinaryOps id allowed type_info sp first ops) =
    SIR.Expr'BinaryOps id allowed type_info sp
        <$> eval_in_expr first
        <*> mapM (\ (sp, iden, (), rhs) -> (sp,,(),) <$> eval_split_iden iden <*> eval_in_expr rhs) ops

eval_in_expr (SIR.Expr'Call id type_info sp callee arg) = SIR.Expr'Call id type_info sp <$> eval_in_expr callee <*> eval_in_expr arg

eval_in_expr (SIR.Expr'If id type_info sp if_sp cond t f) = SIR.Expr'If id type_info sp if_sp <$> eval_in_expr cond <*> eval_in_expr t <*> eval_in_expr f
eval_in_expr (SIR.Expr'Match id type_info sp match_tok_sp e arms) =
    SIR.Expr'Match id type_info sp match_tok_sp
        <$> eval_in_expr e
        <*> mapM (\ (pat, expr) -> (,) <$> eval_in_pat pat <*> eval_in_expr expr) arms

eval_in_expr (SIR.Expr'TypeAnnotation id type_info sp (ty, ()) e) =
    eval_in_type_expr ty >>= \ ty ->
    eval_in_expr e >>= \ e ->
    lift (evaled_as_type ty) >>= \ ty_as_type ->
    pure (SIR.Expr'TypeAnnotation id type_info sp (ty, ty_as_type) e)

eval_in_expr (SIR.Expr'Forall id type_info sp vars e) = SIR.Expr'Forall id type_info sp vars <$> eval_in_expr e
eval_in_expr (SIR.Expr'TypeApply id type_info sp e (arg, ())) = eval_in_expr e >>= \ e -> eval_in_type_expr arg >>= \ arg -> lift (evaled_as_type arg) >>= \ arg_as_type -> pure (SIR.Expr'TypeApply id type_info sp e (arg, arg_as_type))

eval_in_expr (SIR.Expr'Hole id type_info sp hid) = pure $ SIR.Expr'Hole id type_info sp hid

eval_in_expr (SIR.Expr'Poison id type_info sp) = pure $ SIR.Expr'Poison id type_info sp

-- resolving identifiers {{{1
eval_split_iden :: SIR.SplitIdentifier Unevaled start -> Utils.NRReader adt_arena var_arena type_var_arena Utils.SIRChildMaps Utils.WithErrors (SIR.SplitIdentifier Evaled start)
eval_split_iden (SIR.SplitIdentifier'Get texpr next) = eval_in_type_expr texpr >>= \ texpr -> pure (SIR.SplitIdentifier'Get texpr next)
eval_split_iden (SIR.SplitIdentifier'Single start) = pure (SIR.SplitIdentifier'Single start)

evaled_as_type :: EvaledTypeExpr -> Utils.WithErrors (Maybe (Type.Type Void))
evaled_as_type texpr =
    case SIR.type_expr_evaled texpr of
        Just evaled ->
            case evaled of
                SIR.Decl'Module _ -> Compiler.tell_error (Utils.Error'NotAType (SIR.type_expr_span texpr) "a module") >> pure Nothing
                SIR.Decl'Type ty -> pure $ Just ty
        Nothing -> pure Nothing
