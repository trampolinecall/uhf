module UHF.Parts.NameResolve.EvalTypeExprs
    ( eval
    , Unevaled
    , Evaled
    ) where

import UHF.Prelude

import UHF.Source.Located (Located)
import UHF.Source.Span (Span)
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.TypeSolver as TypeSolver
import qualified UHF.Parts.NameResolve.Error as Error
import qualified UHF.Parts.NameResolve.NameMaps as NameMaps
import qualified UHF.Util.Arena as Arena

-- TODO: change errors, clean up this whole module

type EvaledDIden = Maybe (SIR.Decl TypeSolver.Type)
type VIdenStart = Maybe SIR.ValueRef
type PIdenStart = Maybe Type.ADT.VariantIndex

type Unevaled = (EvaledDIden, (), (), VIdenStart, (), PIdenStart, (), (), ())

type UnevaledModuleArena = Arena.Arena (SIR.Module Unevaled) SIR.ModuleKey
type UnevaledADTArena = Arena.Arena (SIR.ADT Unevaled) Type.ADTKey
type UnevaledTypeSynonymArena = Arena.Arena (SIR.TypeSynonym Unevaled) Type.TypeSynonymKey

type Extracted = (EvaledDIden, TypeExprKey, (), VIdenStart, (), PIdenStart, (), (), ())
newtype TypeExprKey = TypeExprKey Arena.KeyData
instance Arena.Key TypeExprKey where
    make_key = TypeExprKey
    unmake_key (TypeExprKey i) = i

type ExtractedModuleArena = Arena.Arena (SIR.Module Extracted) SIR.ModuleKey
type ExtractedADTArena = Arena.Arena (SIR.ADT Extracted) Type.ADTKey
type ExtractedTypeSynonymArena = Arena.Arena (SIR.TypeSynonym Extracted) Type.TypeSynonymKey

type Evaled = (EvaledDIden, EvaledDIden, TypeSolver.Type, VIdenStart, (), PIdenStart, (), (), ())

type EvaledSIR = SIR.SIR Evaled
type EvaledModule = SIR.Module Evaled
type EvaledADT = Type.ADT (EvaledTypeExpr, TypeSolver.Type)
type EvaledTypeSynonym = Type.TypeSynonym (EvaledTypeExpr, TypeSolver.Type)
type EvaledTypeExpr = SIR.TypeExpr Evaled
type EvaledBinding = SIR.Binding Evaled
type EvaledExpr = SIR.Expr Evaled
type EvaledPattern = SIR.Pattern Evaled

type EvaledModuleArena = Arena.Arena EvaledModule SIR.ModuleKey
type EvaledADTArena = Arena.Arena EvaledADT Type.ADTKey
type EvaledTypeSynonymArena = Arena.Arena EvaledTypeSynonym Type.TypeSynonymKey

type QuantVarArena = Arena.Arena Type.QuantVar Type.QuantVarKey

-- extract monad {{{1
type ExtractMonad = State (Arena.Arena TypeExpr TypeExprKey)

make_type_expr_entry :: TypeExpr -> ExtractMonad TypeExprKey
make_type_expr_entry te = state $ \ d_iden_arena ->
    let (k, d_iden_arena') = Arena.put te d_iden_arena
    in (k, d_iden_arena')
-- eval monad {{{1
-- TODO: remove these type parameters?
type EvalMonad adts type_synonyms quant_vars = TypeSolver.SolveMonad (StateT (Arena.Arena (TypeExpr, Maybe EvaledDIden) TypeExprKey) (ReaderT (adts, type_synonyms, quant_vars, NameMaps.SIRChildMaps) (WriterT [TypeSolver.Constraint] Error.WithErrors)))
-- type expr data in arena {{{1
data TypeExpr
    = TypeExpr'Refer Span EvaledDIden
    | TypeExpr'Get Span TypeExprKey (Located Text)
    | TypeExpr'Tuple Span TypeExprKey TypeExprKey
    | TypeExpr'Hole Span SIR.HoleIdentifier
    | TypeExpr'Function Span TypeExprKey TypeExprKey
    | TypeExpr'Forall Span (NonEmpty Type.QuantVarKey) TypeExprKey
    | TypeExpr'Apply Span TypeExprKey TypeExprKey
    | TypeExpr'Wild Span
    | TypeExpr'Poison Span
-- eval {{{1
eval :: NameMaps.SIRChildMaps -> SIR.SIR Unevaled -> Compiler.WithDiagnostics Error.Error Void (EvaledSIR, TypeSolver.SolverState, [TypeSolver.Constraint])
eval sir_child_maps sir =
    let (sir', type_expr_arena) = extract sir
    in put_back sir_child_maps type_expr_arena sir'
-- extract {{{1
extract :: SIR.SIR Unevaled -> (SIR.SIR Extracted, Arena.Arena TypeExpr TypeExprKey)
extract (SIR.SIR mods adts type_synonyms quant_vars variables (SIR.CU root_module main_function)) =
    let ((mods', adts', type_synonyms'), d_iden_arena) =
            runState
                ( do
                    mods <- extract_in_mods mods
                    adts <- extract_in_adts adts
                    type_synonyms <- extract_in_type_synonyms type_synonyms
                    pure (mods, adts, type_synonyms)
                )
                Arena.new
    in (SIR.SIR mods' adts' type_synonyms' quant_vars (Arena.transform change_variable variables) (SIR.CU root_module main_function), d_iden_arena)
    where
        change_variable (SIR.Variable varid tyinfo n) = SIR.Variable varid tyinfo n

extract_in_mods :: UnevaledModuleArena -> ExtractMonad ExtractedModuleArena
extract_in_mods = Arena.transformM extract_in_module

extract_in_adts :: UnevaledADTArena -> ExtractMonad ExtractedADTArena
extract_in_adts = Arena.transformM extract_in_adt

extract_in_type_synonyms :: UnevaledTypeSynonymArena -> ExtractMonad ExtractedTypeSynonymArena
extract_in_type_synonyms = Arena.transformM extract_in_type_synonym

extract_in_module :: SIR.Module Unevaled -> ExtractMonad (SIR.Module Extracted)
extract_in_module (SIR.Module id bindings adts type_synonyms) = SIR.Module id <$> mapM extract_in_binding bindings <*> pure adts <*> pure type_synonyms

extract_in_adt :: SIR.ADT Unevaled -> ExtractMonad (SIR.ADT Extracted)
extract_in_adt (Type.ADT id name quant_vars variants) = Type.ADT id name quant_vars <$> mapM extract_in_variant variants
    where
        extract_in_variant (Type.ADT.Variant'Named name id fields) = Type.ADT.Variant'Named name id <$> mapM (\ (id, name, (ty, ())) -> extract_in_type_expr ty >>= \ (_, ty) -> pure (id, name, (ty, ()))) fields
        extract_in_variant (Type.ADT.Variant'Anon name id fields) = Type.ADT.Variant'Anon name id <$> mapM (\ (id, (ty, ())) -> extract_in_type_expr ty >>= \ (_, ty) -> pure (id, (ty, ()))) fields

extract_in_type_synonym :: SIR.TypeSynonym Unevaled -> ExtractMonad (SIR.TypeSynonym Extracted)
extract_in_type_synonym (Type.TypeSynonym id name (expansion, ())) =
    extract_in_type_expr expansion >>= \ (_, expansion) ->
    pure (Type.TypeSynonym id name (expansion, ()))

extract_in_binding :: SIR.Binding Unevaled -> ExtractMonad (SIR.Binding Extracted)
extract_in_binding (SIR.Binding target eq_sp expr) = SIR.Binding <$> extract_in_pat target <*> pure eq_sp <*> extract_in_expr expr

extract_in_type_expr :: SIR.TypeExpr Unevaled -> ExtractMonad (TypeExprKey, SIR.TypeExpr Extracted)
extract_in_type_expr (SIR.TypeExpr'Refer () sp iden) = do
    k <- make_type_expr_entry (TypeExpr'Refer sp iden)
    pure (k, SIR.TypeExpr'Refer k sp iden)
extract_in_type_expr (SIR.TypeExpr'Get () sp parent name) = do
    (parent_k, parent) <- extract_in_type_expr parent
    k <- make_type_expr_entry (TypeExpr'Get sp parent_k name)
    pure (k, SIR.TypeExpr'Get k sp parent name)
extract_in_type_expr (SIR.TypeExpr'Tuple () sp a b) =
    extract_in_type_expr a >>= \ (a_k, a_conv) ->
    extract_in_type_expr b >>= \ (b_k, b_conv) ->
    make_type_expr_entry (TypeExpr'Tuple sp a_k b_k) >>= \ k ->
    pure (k, SIR.TypeExpr'Tuple k sp a_conv b_conv)
extract_in_type_expr (SIR.TypeExpr'Hole () () sp hid) =
    make_type_expr_entry (TypeExpr'Hole sp hid) >>= \ k ->
    pure (k, SIR.TypeExpr'Hole k () sp hid)
extract_in_type_expr (SIR.TypeExpr'Function () sp arg res) =
    extract_in_type_expr arg >>= \ (arg_k, arg) ->
    extract_in_type_expr res >>= \ (res_k, res) ->
    make_type_expr_entry (TypeExpr'Function sp arg_k res_k) >>= \ k ->
    pure (k, SIR.TypeExpr'Function k sp arg res)
extract_in_type_expr (SIR.TypeExpr'Forall () sp vars inner) =
    extract_in_type_expr inner >>= \ (inner_k, inner) ->
    make_type_expr_entry (TypeExpr'Forall sp vars inner_k) >>= \ k ->
    pure (k, SIR.TypeExpr'Forall k sp vars inner)
extract_in_type_expr (SIR.TypeExpr'Apply () sp ty arg) =
    extract_in_type_expr ty >>= \ (ty_k, ty) ->
    extract_in_type_expr arg >>= \ (arg_k, arg) ->
    make_type_expr_entry (TypeExpr'Apply sp ty_k arg_k) >>= \ k ->
    pure (k, SIR.TypeExpr'Apply k sp ty arg)
extract_in_type_expr (SIR.TypeExpr'Wild () sp) =
    make_type_expr_entry (TypeExpr'Wild sp) >>= \ k ->
    pure (k, SIR.TypeExpr'Wild k sp)
extract_in_type_expr (SIR.TypeExpr'Poison () sp) =
    make_type_expr_entry (TypeExpr'Poison sp) >>= \ k ->
    pure (k, SIR.TypeExpr'Poison k sp)

extract_in_pat :: SIR.Pattern Unevaled -> ExtractMonad (SIR.Pattern Extracted)
extract_in_pat (SIR.Pattern'Variable type_info sp bnk) = pure $ SIR.Pattern'Variable type_info sp bnk
extract_in_pat (SIR.Pattern'Wildcard type_info sp) = pure $ SIR.Pattern'Wildcard type_info sp
extract_in_pat (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp <$> extract_in_pat a <*> extract_in_pat b
extract_in_pat (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk <$> extract_in_pat subpat
extract_in_pat (SIR.Pattern'AnonADTVariant type_info sp variant_split_iden () tyargs subpat) = SIR.Pattern'AnonADTVariant type_info sp <$> extract_in_split_iden variant_split_iden <*> pure () <*> pure tyargs <*> mapM extract_in_pat subpat
extract_in_pat (SIR.Pattern'NamedADTVariant type_info sp variant_split_iden () tyargs subpat) = SIR.Pattern'NamedADTVariant type_info sp <$> extract_in_split_iden variant_split_iden <*> pure () <*> pure tyargs <*> mapM (\ (field_name, field_pat) -> (field_name,) <$> extract_in_pat field_pat) subpat
extract_in_pat (SIR.Pattern'Poison type_info sp) = pure $ SIR.Pattern'Poison type_info sp

extract_in_expr :: SIR.Expr Unevaled -> ExtractMonad (SIR.Expr Extracted)
extract_in_expr (SIR.Expr'Refer id type_info sp iden_split ()) = SIR.Expr'Refer id type_info sp <$> extract_in_split_iden iden_split <*> pure ()
extract_in_expr (SIR.Expr'Char id type_info sp c) = pure $ SIR.Expr'Char id type_info sp c
extract_in_expr (SIR.Expr'String id type_info sp s) = pure $ SIR.Expr'String id type_info sp s
extract_in_expr (SIR.Expr'Int id type_info sp i) = pure $ SIR.Expr'Int id type_info sp i
extract_in_expr (SIR.Expr'Float id type_info sp f) = pure $ SIR.Expr'Float id type_info sp f
extract_in_expr (SIR.Expr'Bool id type_info sp b) = pure $ SIR.Expr'Bool id type_info sp b

extract_in_expr (SIR.Expr'Tuple id type_info sp a b) = SIR.Expr'Tuple id type_info sp <$> extract_in_expr a <*> extract_in_expr b

extract_in_expr (SIR.Expr'Lambda id type_info sp param body) =
    SIR.Expr'Lambda id type_info sp <$> extract_in_pat param <*> extract_in_expr body

extract_in_expr (SIR.Expr'Let id type_info sp bindings adts type_synonyms body) = SIR.Expr'Let id type_info sp <$> mapM extract_in_binding bindings <*> pure adts <*> pure type_synonyms <*> extract_in_expr body
extract_in_expr (SIR.Expr'LetRec id type_info sp bindings adts type_synonyms body) = SIR.Expr'LetRec id type_info sp <$> mapM extract_in_binding bindings <*> pure adts <*> pure type_synonyms <*> extract_in_expr body

extract_in_expr (SIR.Expr'BinaryOps id allowed type_info sp first ops) =
    SIR.Expr'BinaryOps id allowed type_info sp
        <$> extract_in_expr first
        <*> mapM (\ (sp, iden, (), rhs) -> (sp,,,) <$> extract_in_split_iden iden <*> pure () <*> extract_in_expr rhs) ops

extract_in_expr (SIR.Expr'Call id type_info sp callee arg) = SIR.Expr'Call id type_info sp <$> extract_in_expr callee <*> extract_in_expr arg

extract_in_expr (SIR.Expr'If id type_info sp if_sp cond t f) = SIR.Expr'If id type_info sp if_sp <$> extract_in_expr cond <*> extract_in_expr t <*> extract_in_expr f
extract_in_expr (SIR.Expr'Match id type_info sp match_tok_sp e arms) =
    SIR.Expr'Match id type_info sp match_tok_sp
        <$> extract_in_expr e
        <*> mapM (\ (pat, expr) -> (,) <$> extract_in_pat pat <*> extract_in_expr expr) arms

extract_in_expr (SIR.Expr'TypeAnnotation id type_info sp (ty, ()) e) =
    extract_in_type_expr ty >>= \ (_, ty) ->
    extract_in_expr e >>= \ e ->
    pure (SIR.Expr'TypeAnnotation id type_info sp (ty, ()) e)

extract_in_expr (SIR.Expr'Forall id type_info sp vars e) = SIR.Expr'Forall id type_info sp vars <$> extract_in_expr e
extract_in_expr (SIR.Expr'TypeApply id type_info sp e (arg, ())) = extract_in_expr e >>= \ e -> extract_in_type_expr arg >>= \ (_, arg) -> pure (SIR.Expr'TypeApply id type_info sp e (arg, ()))

extract_in_expr (SIR.Expr'Hole id type_info sp hid) = pure $ SIR.Expr'Hole id type_info sp hid

extract_in_expr (SIR.Expr'Poison id type_info sp) = pure $ SIR.Expr'Poison id type_info sp

extract_in_split_iden :: SIR.SplitIdentifier single Unevaled -> ExtractMonad (SIR.SplitIdentifier single Extracted)
extract_in_split_iden (SIR.SplitIdentifier'Get texpr next) = extract_in_type_expr texpr >>= \ (_, texpr) -> pure (SIR.SplitIdentifier'Get texpr next)
extract_in_split_iden (SIR.SplitIdentifier'Single single) = pure (SIR.SplitIdentifier'Single single)
-- put back {{{1
put_back :: NameMaps.SIRChildMaps -> Arena.Arena TypeExpr TypeExprKey -> SIR.SIR Extracted  -> Error.WithErrors (SIR.SIR Evaled, TypeSolver.SolverState, [TypeSolver.Constraint])
put_back sir_child_maps type_expr_arena (SIR.SIR mods adts type_synonyms quant_vars variables (SIR.CU root_module main_function)) = do
    ((sir, solver_state), constraints) <-
            runWriterT (
                runReaderT (
                    evalStateT (
                        TypeSolver.run_solve_monad (
                            -- just put identifiers from arena back into the sir - that process will evaluate (on demand) the type exprs in the arena
                            put_back_in_mods mods >>= \ mods ->
                            put_back_in_adts adts >>= \ adts ->
                            put_back_in_type_synonyms type_synonyms >>= \ synonyms ->
                            pure (SIR.SIR mods adts synonyms quant_vars (Arena.transform change_variable variables) (SIR.CU root_module main_function))
                        )
                    ) (Arena.transform (,Nothing) type_expr_arena)
                ) (adts, type_synonyms, quant_vars, sir_child_maps)
            )
    pure (sir, solver_state, constraints)
    where
        change_variable (SIR.Variable varid tyinfo n) = SIR.Variable varid tyinfo n

put_back_in_mods :: ExtractedModuleArena -> EvalMonad (Arena.Arena (SIR.ADT Extracted) Type.ADTKey) (Arena.Arena (SIR.TypeSynonym Extracted) Type.TypeSynonymKey) QuantVarArena EvaledModuleArena
put_back_in_mods = Arena.transformM put_back_in_module

put_back_in_adts :: ExtractedADTArena -> EvalMonad (Arena.Arena (SIR.ADT Extracted) Type.ADTKey) (Arena.Arena (SIR.TypeSynonym Extracted) Type.TypeSynonymKey) QuantVarArena EvaledADTArena
put_back_in_adts = Arena.transformM put_back_in_adt

put_back_in_type_synonyms :: ExtractedTypeSynonymArena -> EvalMonad (Arena.Arena (SIR.ADT Extracted) Type.ADTKey) (Arena.Arena (SIR.TypeSynonym Extracted) Type.TypeSynonymKey) QuantVarArena EvaledTypeSynonymArena
put_back_in_type_synonyms = Arena.transformM put_back_in_type_synonym

put_back_in_module :: SIR.Module Extracted -> EvalMonad (Arena.Arena (SIR.ADT Extracted) Type.ADTKey) (Arena.Arena (SIR.TypeSynonym Extracted) Type.TypeSynonymKey) QuantVarArena EvaledModule
put_back_in_module (SIR.Module id bindings adts type_synonyms) = SIR.Module id <$> mapM put_back_in_binding bindings <*> pure adts <*> pure type_synonyms

put_back_in_adt :: SIR.ADT Extracted -> EvalMonad (Arena.Arena (SIR.ADT Extracted) Type.ADTKey) (Arena.Arena (SIR.TypeSynonym Extracted) Type.TypeSynonymKey) QuantVarArena EvaledADT
put_back_in_adt (Type.ADT id name quant_vars variants) = Type.ADT id name quant_vars <$> mapM put_back_in_variant variants
    where
        put_back_in_variant (Type.ADT.Variant'Named name id fields) = Type.ADT.Variant'Named name id <$> mapM (\ (id, name, (ty, ())) -> put_back_in_type_expr ty >>= \ ty -> type_expr_evaled_as_type ty >>= \ ty_as_type -> pure (id, name, (ty, ty_as_type))) fields
        put_back_in_variant (Type.ADT.Variant'Anon name id fields) = Type.ADT.Variant'Anon name id <$> mapM (\ (id, (ty, ())) -> put_back_in_type_expr ty >>= \ ty -> type_expr_evaled_as_type ty >>= \ ty_as_type -> pure (id, (ty, ty_as_type))) fields

put_back_in_type_synonym :: SIR.TypeSynonym Extracted -> EvalMonad (Arena.Arena (SIR.ADT Extracted) Type.ADTKey) (Arena.Arena (SIR.TypeSynonym Extracted) Type.TypeSynonymKey) QuantVarArena EvaledTypeSynonym
put_back_in_type_synonym (Type.TypeSynonym id name (expansion, ())) =
    put_back_in_type_expr expansion >>= \ expansion ->
    type_expr_evaled_as_type expansion >>= \ expansion_as_type ->
    pure (Type.TypeSynonym id name (expansion, expansion_as_type))

put_back_in_binding :: SIR.Binding Extracted -> EvalMonad (Arena.Arena (SIR.ADT Extracted) Type.ADTKey) (Arena.Arena (SIR.TypeSynonym Extracted) Type.TypeSynonymKey) QuantVarArena EvaledBinding
put_back_in_binding (SIR.Binding target eq_sp expr) = SIR.Binding <$> put_back_in_pat target <*> pure eq_sp <*> put_back_in_expr expr

put_back_in_type_expr :: SIR.TypeExpr Extracted -> EvalMonad (Arena.Arena (SIR.ADT Extracted) Type.ADTKey) (Arena.Arena (SIR.TypeSynonym Extracted) Type.TypeSynonymKey) QuantVarArena EvaledTypeExpr
put_back_in_type_expr te = case te of
    SIR.TypeExpr'Refer te_k sp iden -> eval_type_expr te_k >>= \ evaled -> pure (SIR.TypeExpr'Refer evaled sp iden)
    SIR.TypeExpr'Get te_k sp parent name -> eval_type_expr te_k >>= \ evaled -> put_back_in_type_expr parent >>= \ parent -> pure (SIR.TypeExpr'Get evaled sp parent name)
    SIR.TypeExpr'Tuple te_k sp a b -> eval_type_expr te_k >>= \ evaled -> put_back_in_type_expr a >>= \ a_conv -> put_back_in_type_expr b >>= \ b_conv -> pure (SIR.TypeExpr'Tuple evaled sp a_conv b_conv)
    SIR.TypeExpr'Hole te_k () sp hid -> eval_type_expr te_k >>= \ evaled -> evaled_as_type sp evaled >>= \ as_ty -> pure (SIR.TypeExpr'Hole evaled as_ty sp hid)
    SIR.TypeExpr'Function te_k sp arg res -> eval_type_expr te_k >>= \ evaled -> put_back_in_type_expr arg >>= \ arg -> put_back_in_type_expr res >>= \ res -> pure (SIR.TypeExpr'Function evaled sp arg res)
    SIR.TypeExpr'Forall te_k sp vars inner -> eval_type_expr te_k >>= \ evaled -> put_back_in_type_expr inner >>= \ inner -> pure (SIR.TypeExpr'Forall evaled sp vars inner)
    SIR.TypeExpr'Apply te_k sp ty arg -> eval_type_expr te_k >>= \ evaled -> put_back_in_type_expr ty >>= \ ty -> put_back_in_type_expr arg >>= \ arg -> pure (SIR.TypeExpr'Apply evaled sp ty arg)
    SIR.TypeExpr'Wild te_k sp -> eval_type_expr te_k >>= \ evaled -> pure (SIR.TypeExpr'Wild evaled sp)
    SIR.TypeExpr'Poison te_k sp -> eval_type_expr te_k >>= \ evaled -> pure (SIR.TypeExpr'Poison evaled sp)

put_back_in_pat :: SIR.Pattern Extracted -> EvalMonad (Arena.Arena (SIR.ADT Extracted) Type.ADTKey) (Arena.Arena (SIR.TypeSynonym Extracted) Type.TypeSynonymKey) QuantVarArena EvaledPattern
put_back_in_pat (SIR.Pattern'Variable type_info sp bnk) = pure $ SIR.Pattern'Variable type_info sp bnk
put_back_in_pat (SIR.Pattern'Wildcard type_info sp) = pure $ SIR.Pattern'Wildcard type_info sp
put_back_in_pat (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp <$> put_back_in_pat a <*> put_back_in_pat b
put_back_in_pat (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk <$> put_back_in_pat subpat
put_back_in_pat (SIR.Pattern'AnonADTVariant type_info sp variant_split_iden () tyargs subpat) = SIR.Pattern'AnonADTVariant type_info sp <$> put_back_split_iden variant_split_iden <*> pure () <*> pure tyargs <*> mapM put_back_in_pat subpat
put_back_in_pat (SIR.Pattern'NamedADTVariant type_info sp variant_split_iden () tyargs subpat) = SIR.Pattern'NamedADTVariant type_info sp <$> put_back_split_iden variant_split_iden <*> pure () <*> pure tyargs <*> mapM (\ (field_name, field_pat) -> (field_name,) <$> put_back_in_pat field_pat) subpat
put_back_in_pat (SIR.Pattern'Poison type_info sp) = pure $ SIR.Pattern'Poison type_info sp

put_back_in_expr :: SIR.Expr Extracted -> EvalMonad (Arena.Arena (SIR.ADT Extracted) Type.ADTKey) (Arena.Arena (SIR.TypeSynonym Extracted) Type.TypeSynonymKey) QuantVarArena EvaledExpr
put_back_in_expr (SIR.Expr'Refer id type_info sp iden_split ()) = SIR.Expr'Refer id type_info sp <$> put_back_split_iden iden_split <*> pure ()
put_back_in_expr (SIR.Expr'Char id type_info sp c) = pure $ SIR.Expr'Char id type_info sp c
put_back_in_expr (SIR.Expr'String id type_info sp s) = pure $ SIR.Expr'String id type_info sp s
put_back_in_expr (SIR.Expr'Int id type_info sp i) = pure $ SIR.Expr'Int id type_info sp i
put_back_in_expr (SIR.Expr'Float id type_info sp f) = pure $ SIR.Expr'Float id type_info sp f
put_back_in_expr (SIR.Expr'Bool id type_info sp b) = pure $ SIR.Expr'Bool id type_info sp b

put_back_in_expr (SIR.Expr'Tuple id type_info sp a b) = SIR.Expr'Tuple id type_info sp <$> put_back_in_expr a <*> put_back_in_expr b

put_back_in_expr (SIR.Expr'Lambda id type_info sp param body) =
    SIR.Expr'Lambda id type_info sp <$> put_back_in_pat param <*> put_back_in_expr body

put_back_in_expr (SIR.Expr'Let id type_info sp bindings adts type_synonyms body) = SIR.Expr'Let id type_info sp <$> mapM put_back_in_binding bindings <*> pure adts <*> pure type_synonyms <*> put_back_in_expr body
put_back_in_expr (SIR.Expr'LetRec id type_info sp bindings adts type_synonyms body) = SIR.Expr'LetRec id type_info sp <$> mapM put_back_in_binding bindings <*> pure adts <*> pure type_synonyms <*> put_back_in_expr body

put_back_in_expr (SIR.Expr'BinaryOps id allowed type_info sp first ops) =
    SIR.Expr'BinaryOps id allowed type_info sp
        <$> put_back_in_expr first
        <*> mapM (\ (sp, iden, (), rhs) -> (sp,,(),) <$> put_back_split_iden iden <*> put_back_in_expr rhs) ops

put_back_in_expr (SIR.Expr'Call id type_info sp callee arg) = SIR.Expr'Call id type_info sp <$> put_back_in_expr callee <*> put_back_in_expr arg

put_back_in_expr (SIR.Expr'If id type_info sp if_sp cond t f) = SIR.Expr'If id type_info sp if_sp <$> put_back_in_expr cond <*> put_back_in_expr t <*> put_back_in_expr f
put_back_in_expr (SIR.Expr'Match id type_info sp match_tok_sp e arms) =
    SIR.Expr'Match id type_info sp match_tok_sp
        <$> put_back_in_expr e
        <*> mapM (\ (pat, expr) -> (,) <$> put_back_in_pat pat <*> put_back_in_expr expr) arms

put_back_in_expr (SIR.Expr'TypeAnnotation id type_info sp (ty, ()) e) =
    put_back_in_type_expr ty >>= \ ty ->
    put_back_in_expr e >>= \ e ->
    type_expr_evaled_as_type ty >>= \ ty_as_type ->
    pure (SIR.Expr'TypeAnnotation id type_info sp (ty, ty_as_type) e)

put_back_in_expr (SIR.Expr'Forall id type_info sp vars e) = SIR.Expr'Forall id type_info sp vars <$> put_back_in_expr e
put_back_in_expr (SIR.Expr'TypeApply id type_info sp e (arg, ())) = put_back_in_expr e >>= \ e -> put_back_in_type_expr arg >>= \ arg -> type_expr_evaled_as_type arg >>= \ arg_as_type -> pure (SIR.Expr'TypeApply id type_info sp e (arg, arg_as_type))

put_back_in_expr (SIR.Expr'Hole id type_info sp hid) = pure $ SIR.Expr'Hole id type_info sp hid

put_back_in_expr (SIR.Expr'Poison id type_info sp) = pure $ SIR.Expr'Poison id type_info sp

put_back_split_iden :: SIR.SplitIdentifier single Extracted -> EvalMonad (Arena.Arena (SIR.ADT Extracted) Type.ADTKey) (Arena.Arena (SIR.TypeSynonym Extracted) Type.TypeSynonymKey) QuantVarArena (SIR.SplitIdentifier single Evaled)
put_back_split_iden (SIR.SplitIdentifier'Get texpr next) = put_back_in_type_expr texpr >>= \ texpr -> pure (SIR.SplitIdentifier'Get texpr next)
put_back_split_iden (SIR.SplitIdentifier'Single single) = pure (SIR.SplitIdentifier'Single single)

make_infer_var :: TypeSolver.InferVarForWhat -> EvalMonad adts type_synonyms quant_vars TypeSolver.Type
make_infer_var for_what = TypeSolver.Type'InferVar <$> TypeSolver.new_infer_var for_what

-- TODO: prevent infinite recursion with loops in type synonyms or loops in imports (when imports get added)
eval_type_expr :: TypeExprKey -> EvalMonad (Arena.Arena (SIR.ADT Extracted) Type.ADTKey) (Arena.Arena (SIR.TypeSynonym Extracted) Type.TypeSynonymKey) QuantVarArena EvaledDIden
eval_type_expr tek = do
    (te, evaled) <- lift $ Arena.get <$> get <*> pure tek
    case evaled of
        Just result -> pure result
        Nothing -> do
            evaled <- go te
            lift $ modify
                (\ arena ->
                    Arena.modify
                        arena
                        tek
                        (\ (t_expr, m_evaled) ->
                            case m_evaled of
                                Nothing -> (t_expr, Just evaled)
                                Just _ -> (t_expr, Just evaled) -- TODO: log / warn (internally) if m_evaled is Just because that means that a type expression was evaluated more than once
                        )
                )
            pure evaled
    where
        go :: TypeExpr -> EvalMonad (Arena.Arena (Type.ADT.ADT (SIR.TypeExpr Extracted, ())) Type.ADT.ADTKey) (Arena.Arena (Type.TypeSynonym (SIR.TypeExpr Extracted, ())) Type.TypeSynonymKey) QuantVarArena EvaledDIden
        go (TypeExpr'Refer _ iden) = pure iden
        go (TypeExpr'Get _ parent name) = do
            (_, _, _, sir_child_maps) <- lift $ lift ask
            parent_evaled <- eval_type_expr parent
            case parent_evaled of
                Just parent -> case NameMaps.get_decl_child sir_child_maps parent name of
                    Right r -> pure $ Just r
                    Left e -> lift (lift $ lift $ lift $ Compiler.tell_error e) >> pure Nothing
                Nothing -> pure Nothing

        go (TypeExpr'Tuple _ a b) =
            eval_type_expr a >>= \ a_evaled ->
            eval_type_expr b >>= \ b_evaled ->
            evaled_as_type' a a_evaled >>= \ a_as_type ->
            evaled_as_type' b b_evaled >>= \ b_as_type ->
            pure (Just $ SIR.Decl'Type $ TypeSolver.Type'Tuple a_as_type b_as_type)
        go (TypeExpr'Hole sp _) =
            make_infer_var (TypeSolver.TypeHole sp) >>= \ infer_var ->
            pure (Just $ SIR.Decl'Type infer_var)
        go (TypeExpr'Function _ arg res) =
            eval_type_expr arg >>= \ arg_evaled ->
            eval_type_expr res >>= \ res_evaled ->
            evaled_as_type' arg arg_evaled >>= \ arg_as_type ->
            evaled_as_type' res res_evaled >>= \ res_as_type ->
            pure (Just $ SIR.Decl'Type $ TypeSolver.Type'Function arg_as_type res_as_type)
        go (TypeExpr'Forall _ vars inner) =
            eval_type_expr inner >>= \ inner_evaled ->
            evaled_as_type' inner inner_evaled >>= \ inner_as_type ->
            pure (Just $ SIR.Decl'Type $ TypeSolver.Type'Forall vars inner_as_type)
        go (TypeExpr'Apply sp ty arg) =
            eval_type_expr ty >>= \ ty_evaled ->
            eval_type_expr arg >>= \ arg_evaled ->
            evaled_as_type' ty ty_evaled >>= \ ty_as_type ->
            evaled_as_type' arg arg_evaled >>= \ arg_as_type ->
            lift (lift ask) >>= \ (adts, type_synonyms, quant_vars, _) ->
            TypeSolver.apply_type adts type_synonyms (get_type_synonym type_synonyms) quant_vars (TypeSolver.TypeExpr sp) sp ty_as_type arg_as_type >>= (\case
                TypeSolver.AppliedResult res -> pure res
                TypeSolver.AppliedError err -> do
                    _ <- lift $ lift $ lift $ lift $ Compiler.tell_error (Error.Error'SolveError err)
                    result_ifv <- make_infer_var (TypeSolver.TypeExpr sp) -- TODO: fix duplication of this for_what
                    pure result_ifv
                TypeSolver.Inconclusive ty constraint -> do
                    lift $ tell [constraint]
                    pure ty) >>= \ result_ty ->

            pure (Just $ SIR.Decl'Type result_ty)
        go (TypeExpr'Wild sp) =
            make_infer_var (TypeSolver.TypeExpr sp) >>= \ infer_var ->
            pure (Just $ SIR.Decl'Type infer_var)
        go (TypeExpr'Poison sp) =
            make_infer_var (TypeSolver.TypeExpr sp) >>= \ infer_var ->
            pure (Just $ SIR.Decl'Type infer_var)

        get_type_synonym ts_arena ts_k =
            let type_synonym = Arena.get ts_arena ts_k
            in put_back_in_type_synonym type_synonym

        type_expr_span te_k = do
            (te, _) <- lift $ Arena.get <$> get <*> pure te_k
            pure $
                case te of
                    TypeExpr'Refer sp _ -> sp
                    TypeExpr'Get sp _ _ -> sp
                    TypeExpr'Tuple sp _ _ -> sp
                    TypeExpr'Hole sp _ -> sp
                    TypeExpr'Function sp _ _ -> sp
                    TypeExpr'Forall sp _ _ -> sp
                    TypeExpr'Apply sp _ _ -> sp
                    TypeExpr'Wild sp -> sp
                    TypeExpr'Poison sp -> sp

        evaled_as_type' te_k evaled = type_expr_span te_k >>= \ sp -> evaled_as_type sp evaled

type_expr_evaled_as_type :: SIR.TypeExpr Evaled -> EvalMonad adts type_synonyms quant_vars TypeSolver.Type
type_expr_evaled_as_type te = evaled_as_type (SIR.type_expr_span te) (SIR.type_expr_evaled te)

evaled_as_type :: Span -> Maybe (SIR.Decl TypeSolver.Type) -> EvalMonad adts type_synonyms quant_vars TypeSolver.Type
evaled_as_type sp decl =
    case decl of
        Just (SIR.Decl'Module _) -> lift (lift $ lift $ lift $ Compiler.tell_error (Error.Error'NotAType sp "a module")) >> make_infer_var (TypeSolver.TypeExpr sp) -- TODO: don't make variables for these?
        Just (SIR.Decl'Type ty) -> pure ty
        Just (SIR.Decl'ExternPackage _) -> lift (lift $ lift $ lift $ Compiler.tell_error (Error.Error'NotAType sp "external package")) >> make_infer_var (TypeSolver.TypeExpr sp) -- TODO: don't make variables for these?
        Nothing -> make_infer_var (TypeSolver.TypeExpr sp) -- TODO: make this message better
