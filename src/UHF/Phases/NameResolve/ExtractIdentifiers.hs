module UHF.Phases.NameResolve.ExtractIdentifiers
    ( extract
    , Inline
    , Extracted
    ) where

import UHF.Prelude

import UHF.Phases.NameResolve.IdenKeys
import UHF.Source.Located (Located)
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import qualified UHF.Util.Arena as Arena

-- TODO: change errors, clean up this whole module

type IdenStart = Located Text

type Inline = (IdenStart, (), (), IdenStart, (), IdenStart, (), (), ())

-- TODO: remove these type aliases
type InlineSIR = SIR.SIR Inline
type InlineModule = SIR.Module Inline
type InlineADT = Type.ADT (InlineTypeExpr, ())
type InlineTypeSynonym = Type.TypeSynonym (InlineTypeExpr, ())
type InlineTypeExpr = SIR.TypeExpr Inline
type InlineBinding = SIR.Binding Inline
type InlineExpr = SIR.Expr Inline
type InlinePattern = SIR.Pattern Inline

type InlineModuleArena = Arena.Arena InlineModule SIR.ModuleKey
type InlineADTArena = Arena.Arena InlineADT Type.ADTKey
type InlineTypeSynonymArena = Arena.Arena InlineTypeSynonym Type.TypeSynonymKey

type Extracted = (DIdenStartKey, DIdenKey, (), VIdenStartKey, VIdenKey, PIdenStartKey, PIdenKey, (), ())

type ExtractedSIR = SIR.SIR Extracted
type ExtractedModule = SIR.Module Extracted
type ExtractedADT = Type.ADT (ExtractedTypeExpr, ())
type ExtractedTypeSynonym = Type.TypeSynonym (ExtractedTypeExpr, ())
type ExtractedTypeExpr = SIR.TypeExpr Extracted
type ExtractedBinding = SIR.Binding Extracted
type ExtractedExpr = SIR.Expr Extracted
type ExtractedPattern = SIR.Pattern Extracted

type ExtractedModuleArena = Arena.Arena ExtractedModule SIR.ModuleKey
type ExtractedADTArena = Arena.Arena ExtractedADT Type.ADTKey
type ExtractedTypeSynonymArena = Arena.Arena ExtractedTypeSynonym Type.TypeSynonymKey

type DIdenStartArena d = Arena.Arena d DIdenStartKey
type DIdenArena d = Arena.Arena d DIdenKey
type VIdenStartArena d = Arena.Arena d VIdenStartKey
type VIdenArena d = Arena.Arena d VIdenKey
type PIdenStartArena d = Arena.Arena d PIdenStartKey
type PIdenArena d = Arena.Arena d PIdenKey

-- monad {{{
type ExtractMonad = State (DIdenStartArena IdenStart, DIdenArena (), VIdenStartArena IdenStart, VIdenArena (), PIdenStartArena IdenStart, PIdenArena ())

-- TODO: make this less repetitive?

make_d_iden_start_entry :: IdenStart -> ExtractMonad DIdenStartKey
make_d_iden_start_entry iden = state $ \ (d_iden_start_arena, d_iden_arena, v_iden_start_arena, v_iden_arena, p_iden_start_arena, p_iden_arena) ->
    let (k, d_iden_start_arena') = Arena.put iden d_iden_start_arena
    in (k, (d_iden_start_arena', d_iden_arena, v_iden_start_arena, v_iden_arena, p_iden_start_arena, p_iden_arena))
make_v_iden_start_entry :: IdenStart -> ExtractMonad VIdenStartKey
make_v_iden_start_entry iden = state $ \ (d_iden_start_arena, d_iden_arena, v_iden_start_arena, v_iden_arena, p_iden_start_arena, p_iden_arena) ->
    let (k, v_iden_start_arena') = Arena.put iden v_iden_start_arena
    in (k, (d_iden_start_arena, d_iden_arena, v_iden_start_arena', v_iden_arena, p_iden_start_arena, p_iden_arena))
make_p_iden_start_entry :: IdenStart -> ExtractMonad PIdenStartKey
make_p_iden_start_entry iden = state $ \ (d_iden_start_arena, d_iden_arena, v_iden_start_arena, v_iden_arena, p_iden_start_arena, p_iden_arena) ->
    let (k, p_iden_start_arena') = Arena.put iden p_iden_start_arena
    in (k, (d_iden_start_arena, d_iden_arena, v_iden_start_arena, v_iden_arena, p_iden_start_arena', p_iden_arena))

make_d_iden_entry :: ExtractMonad DIdenKey
make_d_iden_entry = state $ \ (d_iden_start_arena, d_iden_arena, v_iden_start_arena, v_iden_arena, p_iden_start_arena, p_iden_arena) ->
    let (k, d_iden_arena') = Arena.put () d_iden_arena
    in (k, (d_iden_start_arena, d_iden_arena', v_iden_start_arena, v_iden_arena, p_iden_start_arena, p_iden_arena))
make_v_iden_entry :: ExtractMonad VIdenKey
make_v_iden_entry = state $ \ (d_iden_start_arena, d_iden_arena, v_iden_start_arena, v_iden_arena, p_iden_start_arena, p_iden_arena) ->
    let (k, v_iden_arena') = Arena.put () v_iden_arena
    in (k, (d_iden_start_arena, d_iden_arena, v_iden_start_arena, v_iden_arena', p_iden_start_arena, p_iden_arena))
make_p_iden_entry :: ExtractMonad PIdenKey
make_p_iden_entry = state $ \ (d_iden_start_arena, d_iden_arena, v_iden_start_arena, v_iden_arena, p_iden_start_arena, p_iden_arena) ->
    let (k, p_iden_arena') = Arena.put () p_iden_arena
    in (k, (d_iden_start_arena, d_iden_arena, v_iden_start_arena, v_iden_arena, p_iden_start_arena, p_iden_arena'))
-- }}}

extract :: InlineSIR -> (ExtractedSIR, DIdenStartArena IdenStart, DIdenArena (), VIdenStartArena IdenStart, VIdenArena (), PIdenStartArena IdenStart, PIdenArena ())
extract (SIR.SIR mods adts type_synonyms quant_vars variables mod) =
    let ((mods', adts', type_synonyms'), (d_iden_start_arena, d_iden_arena, v_iden_start_arena, v_iden_arena, p_iden_start_arena, p_iden_arena)) =
            runState
                ( do
                    mods <- extract_in_mods mods
                    adts <- extract_in_adts adts
                    type_synonyms <- extract_in_type_synonyms type_synonyms
                    pure (mods, adts, type_synonyms)
                )
                (Arena.new, Arena.new, Arena.new, Arena.new, Arena.new, Arena.new)
    in (SIR.SIR mods' adts' type_synonyms' quant_vars (Arena.transform change_variable variables) mod, d_iden_start_arena, d_iden_arena, v_iden_start_arena, v_iden_arena, p_iden_start_arena, p_iden_arena)
    where
        change_variable (SIR.Variable varid tyinfo n) = SIR.Variable varid tyinfo n
        change_variable (SIR.Variable'ADTVariant varid id tyvars tyinfo sp) = SIR.Variable'ADTVariant varid id tyvars tyinfo sp

extract_in_mods :: InlineModuleArena -> ExtractMonad ExtractedModuleArena
extract_in_mods = Arena.transformM extract_in_module

extract_in_adts :: InlineADTArena -> ExtractMonad ExtractedADTArena
extract_in_adts = Arena.transformM extract_in_adt

extract_in_type_synonyms :: InlineTypeSynonymArena -> ExtractMonad ExtractedTypeSynonymArena
extract_in_type_synonyms = Arena.transformM extract_in_type_synonym

extract_in_module :: InlineModule -> ExtractMonad ExtractedModule
extract_in_module (SIR.Module id bindings adts type_synonyms) = SIR.Module id <$> mapM extract_in_binding bindings <*> pure adts <*> pure type_synonyms

extract_in_adt :: InlineADT -> ExtractMonad ExtractedADT
extract_in_adt (Type.ADT id name quant_vars variants) = Type.ADT id name quant_vars <$> mapM extract_in_variant variants
    where
        extract_in_variant (Type.ADT.Variant'Named name id fields) = Type.ADT.Variant'Named name id <$> mapM (\ (id, name, (ty, ())) -> extract_in_type_expr ty >>= \ ty -> pure (id, name, (ty, ()))) fields
        extract_in_variant (Type.ADT.Variant'Anon name id fields) = Type.ADT.Variant'Anon name id <$> mapM (\ (id, (ty, ())) -> extract_in_type_expr ty >>= \ ty -> pure (id, (ty, ()))) fields

extract_in_type_synonym :: InlineTypeSynonym -> ExtractMonad ExtractedTypeSynonym
extract_in_type_synonym (Type.TypeSynonym id name (expansion, ())) =
    extract_in_type_expr expansion >>= \ expansion ->
    pure (Type.TypeSynonym id name (expansion, ()))

extract_in_binding :: InlineBinding -> ExtractMonad ExtractedBinding
extract_in_binding (SIR.Binding target eq_sp expr) = SIR.Binding <$> extract_in_pat target <*> pure eq_sp <*> extract_in_expr expr
extract_in_binding (SIR.Binding'ADTVariant var_key variant vars sp) = pure $ SIR.Binding'ADTVariant var_key variant vars sp

extract_in_type_expr :: InlineTypeExpr -> ExtractMonad ExtractedTypeExpr
extract_in_type_expr (SIR.TypeExpr'Refer () sp iden) = do
    iden <- make_d_iden_start_entry iden
    k <- make_d_iden_entry
    pure (SIR.TypeExpr'Refer k sp iden)
extract_in_type_expr (SIR.TypeExpr'Get () sp parent name) = do
    parent_conv <- extract_in_type_expr parent
    k <- make_d_iden_entry
    pure (SIR.TypeExpr'Get k sp parent_conv name)
extract_in_type_expr (SIR.TypeExpr'Tuple () sp a b) =
    make_d_iden_entry >>= \ k ->
    extract_in_type_expr a >>= \ a_conv ->
    extract_in_type_expr b >>= \ b_conv ->
    pure (SIR.TypeExpr'Tuple k sp a_conv b_conv)
extract_in_type_expr (SIR.TypeExpr'Hole () () sp hid) =
    make_d_iden_entry >>= \ k ->
    pure (SIR.TypeExpr'Hole k () sp hid)
extract_in_type_expr (SIR.TypeExpr'Function () sp arg res) =
    make_d_iden_entry >>= \ k ->
    extract_in_type_expr arg >>= \ arg ->
    extract_in_type_expr res >>= \ res ->
    pure (SIR.TypeExpr'Function k sp arg res)
extract_in_type_expr (SIR.TypeExpr'Forall () sp vars inner) =
    make_d_iden_entry >>= \ k ->
    extract_in_type_expr inner >>= \ inner ->
    pure (SIR.TypeExpr'Forall k sp vars inner)
extract_in_type_expr (SIR.TypeExpr'Apply () sp ty arg) =
    make_d_iden_entry >>= \ k ->
    extract_in_type_expr ty >>= \ ty ->
    extract_in_type_expr arg >>= \ arg ->
    pure (SIR.TypeExpr'Apply k sp ty arg)
extract_in_type_expr (SIR.TypeExpr'Wild () sp) =
    make_d_iden_entry >>= \ k ->
    pure (SIR.TypeExpr'Wild k sp)
extract_in_type_expr (SIR.TypeExpr'Poison () sp) =
    make_d_iden_entry >>= \ k ->
    pure (SIR.TypeExpr'Poison k sp)

extract_in_pat :: InlinePattern -> ExtractMonad ExtractedPattern
extract_in_pat (SIR.Pattern'Identifier type_info sp bnk) = pure $ SIR.Pattern'Identifier type_info sp bnk
extract_in_pat (SIR.Pattern'Wildcard type_info sp) = pure $ SIR.Pattern'Wildcard type_info sp
extract_in_pat (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp <$> extract_in_pat a <*> extract_in_pat b
extract_in_pat (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk <$> extract_in_pat subpat
extract_in_pat (SIR.Pattern'AnonADTVariant type_info sp variant_split_iden () tyargs subpat) = SIR.Pattern'AnonADTVariant type_info sp <$> extract_in_split_iden make_p_iden_start_entry variant_split_iden <*> make_p_iden_entry <*> pure tyargs <*> mapM extract_in_pat subpat
extract_in_pat (SIR.Pattern'NamedADTVariant type_info sp variant_split_iden () tyargs subpat) = SIR.Pattern'NamedADTVariant type_info sp <$> extract_in_split_iden make_p_iden_start_entry variant_split_iden <*> make_p_iden_entry <*> pure tyargs <*> mapM (\ (field_name, field_pat) -> (field_name,) <$> extract_in_pat field_pat) subpat
extract_in_pat (SIR.Pattern'Poison type_info sp) = pure $ SIR.Pattern'Poison type_info sp

extract_in_expr :: InlineExpr -> ExtractMonad ExtractedExpr
extract_in_expr (SIR.Expr'Identifier id type_info sp iden_split ()) = SIR.Expr'Identifier id type_info sp <$> extract_in_split_iden make_v_iden_start_entry iden_split <*> make_v_iden_entry
extract_in_expr (SIR.Expr'Char id type_info sp c) = pure $ SIR.Expr'Char id type_info sp c
extract_in_expr (SIR.Expr'String id type_info sp s) = pure $ SIR.Expr'String id type_info sp s
extract_in_expr (SIR.Expr'Int id type_info sp i) = pure $ SIR.Expr'Int id type_info sp i
extract_in_expr (SIR.Expr'Float id type_info sp f) = pure $ SIR.Expr'Float id type_info sp f
extract_in_expr (SIR.Expr'Bool id type_info sp b) = pure $ SIR.Expr'Bool id type_info sp b

extract_in_expr (SIR.Expr'Tuple id type_info sp a b) = SIR.Expr'Tuple id type_info sp <$> extract_in_expr a <*> extract_in_expr b

extract_in_expr (SIR.Expr'Lambda id type_info sp param body) =
    SIR.Expr'Lambda id type_info sp <$> extract_in_pat param <*> extract_in_expr body

extract_in_expr (SIR.Expr'Let id type_info sp bindings body) = SIR.Expr'Let id type_info sp <$> mapM extract_in_binding bindings <*> extract_in_expr body
extract_in_expr (SIR.Expr'LetRec id type_info sp bindings body) = SIR.Expr'LetRec id type_info sp <$> mapM extract_in_binding bindings <*> extract_in_expr body

extract_in_expr (SIR.Expr'BinaryOps id allowed type_info sp first ops) =
    SIR.Expr'BinaryOps id allowed type_info sp
        <$> extract_in_expr first
        <*> mapM (\ (sp, iden, (), rhs) -> (sp,,,) <$> extract_in_split_iden make_v_iden_start_entry iden <*> make_v_iden_entry <*> extract_in_expr rhs) ops

extract_in_expr (SIR.Expr'Call id type_info sp callee arg) = SIR.Expr'Call id type_info sp <$> extract_in_expr callee <*> extract_in_expr arg

extract_in_expr (SIR.Expr'If id type_info sp if_sp cond t f) = SIR.Expr'If id type_info sp if_sp <$> extract_in_expr cond <*> extract_in_expr t <*> extract_in_expr f
extract_in_expr (SIR.Expr'Match id type_info sp match_tok_sp e arms) =
    SIR.Expr'Match id type_info sp match_tok_sp
        <$> extract_in_expr e
        <*> mapM (\ (pat, expr) -> (,) <$> extract_in_pat pat <*> extract_in_expr expr) arms

extract_in_expr (SIR.Expr'TypeAnnotation id type_info sp (ty, ()) e) =
    extract_in_type_expr ty >>= \ ty ->
    extract_in_expr e >>= \ e ->
    pure (SIR.Expr'TypeAnnotation id type_info sp (ty, ()) e)

extract_in_expr (SIR.Expr'Forall id type_info sp vars e) = SIR.Expr'Forall id type_info sp vars <$> extract_in_expr e
extract_in_expr (SIR.Expr'TypeApply id type_info sp e (arg, ())) = extract_in_expr e >>= \ e -> extract_in_type_expr arg >>= \ arg -> pure (SIR.Expr'TypeApply id type_info sp e (arg, ()))

extract_in_expr (SIR.Expr'Hole id type_info sp hid) = pure $ SIR.Expr'Hole id type_info sp hid

extract_in_expr (SIR.Expr'Poison id type_info sp) = pure $ SIR.Expr'Poison id type_info sp

extract_in_split_iden :: (IdenStart -> ExtractMonad key) -> SIR.SplitIdentifier Inline IdenStart -> ExtractMonad (SIR.SplitIdentifier Extracted key)
extract_in_split_iden _ (SIR.SplitIdentifier'Get texpr next) = extract_in_type_expr texpr >>= \ texpr -> pure (SIR.SplitIdentifier'Get texpr next)
extract_in_split_iden make_key (SIR.SplitIdentifier'Single start) = make_key start >>= \ k -> pure (SIR.SplitIdentifier'Single k)
