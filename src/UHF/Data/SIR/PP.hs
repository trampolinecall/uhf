{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

module UHF.Data.SIR.PP (dump_main_module) where

import UHF.Prelude

import UHF.Source.Located (Located (Located, unlocate))
import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.PP as Type.PP
import qualified UHF.Data.SIR as SIR
import qualified UHF.PP as PP
import qualified UHF.PP.Precedence as PP.Precedence
import qualified UHF.Util.Arena as Arena
import qualified UHF.Data.IR.TypeWithInferVar as TypeWithInferVar
import qualified UHF.Data.IR.TypeWithInferVar.PP as TypeWithInferVar.PP
import qualified UHF.Data.IR.TypeWithInferVar.PP.InferVarNamer as TypeWithInferVar.PP.InferVarNamer
import qualified UHF.Data.SIR.ID as SIR.ID

type IRReader stage = Reader (SIR.SIR stage)

type DumpableConstraints stage = (Show Int)
    -- ( DumpableIdentifier stage (SIR.DIdenStart stage)
    -- , DumpableIdentifier stage (SIR.VIdenStart stage)
    -- , DumpableIdentifier stage (SIR.PIdenStart stage)
    -- )

-- TODO: be able to print resolved identifiers here (just rewrite this entire module?)

dump_main_module :: DumpableConstraints stage => SIR.SIR stage -> Text
dump_main_module ir@(SIR.SIR modules _ _ _ _ (SIR.CU root_module _)) = PP.render $ runReader (define_module $ Arena.get modules root_module) ir

get_adt_arena :: IRReader stage (Arena.Arena (Type.ADT (SIR.TypeExpr stage, SIR.ID.ID "TypeExprEvaledAsType")) Type.ADTKey)
get_adt_arena = reader (\ (SIR.SIR _ adts _ _ _ _) -> adts)
get_type_synonym_arena :: IRReader stage (Arena.Arena (Type.TypeSynonym (SIR.TypeExpr stage, SIR.ID.ID "TypeExprEvaledAsType")) Type.TypeSynonymKey)
get_type_synonym_arena = reader (\ (SIR.SIR _ _ syns _ _ _) -> syns)
get_quant_var_arena :: IRReader stage (Arena.Arena Type.QuantVar Type.QuantVarKey)
get_quant_var_arena = reader (\ (SIR.SIR _ _ _ vars _ _) -> vars)

get_var :: SIR.VariableKey -> IRReader stage (SIR.Variable stage)
get_var k = reader (\ (SIR.SIR _ _ _ _ vars _) -> Arena.get vars k)
get_module :: SIR.ModuleKey -> IRReader stage (SIR.Module stage)
get_module k = reader (\ (SIR.SIR modules _ _ _ _ _) -> Arena.get modules k)
get_adt :: Type.ADTKey -> IRReader stage (Type.ADT (SIR.TypeExpr stage, SIR.ID.ID "TypeExprEvaledAsType"))
get_adt k = reader (\ (SIR.SIR _ adts _ _ _ _) -> Arena.get adts k)
get_type_syn :: Type.TypeSynonymKey -> IRReader stage (Type.TypeSynonym (SIR.TypeExpr stage, SIR.ID.ID "TypeExprEvaledAsType"))
get_type_syn k = reader (\ (SIR.SIR _ _ syns _ _ _) -> Arena.get syns k)
get_quant_var :: Type.QuantVarKey -> IRReader stage Type.QuantVar
get_quant_var k = reader (\ (SIR.SIR _ _ _ quant_vars _ _) -> Arena.get quant_vars k)

define_module :: DumpableConstraints stage => SIR.Module stage -> IRReader stage PP.Token
define_module (SIR.Module _ _ _ bindings adts type_synonyms) =
    ask >>= \ sir ->
    get_quant_var_arena >>= \ quant_var_arena ->
    mapM (\ k -> get_adt k >>= \ adt -> pure (Type.PP.define_adt quant_var_arena (\ (ty, _) -> runReader (type_expr ty) sir) adt)) adts >>= \ adts_defined ->
    mapM (\ k -> get_type_syn k >>= \ ts -> pure (Type.PP.define_type_synonym (\ (ty, _) -> runReader (type_expr ty) sir) ts)) type_synonyms >>= \ type_synonyms_defined ->
    mapM define_binding bindings >>= \ bindings_defined ->
    pure (PP.flat_block $ adts_defined <> type_synonyms_defined <> bindings_defined)

define_binding :: DumpableConstraints stage => SIR.Binding stage -> IRReader stage PP.Token
define_binding (SIR.Binding _ pat _ init) = pattern pat >>= \ pat -> expr init >>= \ init -> pure $ PP.List [pat, " = ", init, ";"]

refer_var :: SIR.VariableKey -> IRReader stage PP.Token
refer_var k = get_var k >>= \case
    SIR.Variable _ id _ -> pure $ PP.String (ID.stringify id)

-- TODO: rename this to something better
-- TODO: remove this and print it through annotations
-- refer_bv :: SIR.ValueRef -> IRReader stage PP.Token
-- refer_bv (SIR.ValueRef'Variable v) = refer_var v
-- refer_bv (SIR.ValueRef'ADTVariantConstructor var) = refer_iden var
-- refer_bv (SIR.ValueRef'Intrinsic i) = pure $ PP.String $ Intrinsics.intrinsic_name i

-- TODO: rename this to something better
-- -- TODO: remove this and print it through annotations
-- refer_decl :: DumpableType stage t => SIR.DeclRef t -> IRReader stage PP.Token
-- refer_decl d = case d of
--     SIR.DeclRef'Module m ->
--         get_module m >>= \ (SIR.Module _ id _ _ _ _) ->
--         pure (PP.String $ ID.stringify id)
--     SIR.DeclRef'Type ty -> refer_type ty
--     SIR.DeclRef'ExternPackage SIR.ExternPackage'IntrinsicsPackage -> pure "uhf_intrinsics"

-- TODO: remove this and print it through annotations
-- refer_adt_variant :: Type.ADT.VariantIndex -> IRReader stage PP.Token
-- refer_adt_variant variant_index@(Type.ADT.VariantIndex _ adt_key _) =
--     Type.PP.refer_adt <$> get_adt adt_key >>= \ adt_referred ->
--     Type.ADT.get_variant <$> get_adt_arena <*> pure variant_index >>= \ variant ->
--     let variant_name = unlocate $ Type.ADT.variant_name variant
--     in pure $ PP.List [adt_referred, "::", PP.String variant_name]

class DumpableType stage ty where
    refer_type :: ty -> IRReader stage PP.Token

instance DumpableType stage TypeWithInferVar.Type where
    refer_type t = do
        adt_arena <- get_adt_arena
        type_synonym_arena <- get_type_synonym_arena
        quant_var_arena <- get_quant_var_arena
        pure (fst $ TypeWithInferVar.PP.InferVarNamer.run_infer_var_namer $ TypeWithInferVar.PP.pp_type False adt_arena type_synonym_arena quant_var_arena todo t) -- TODO
instance DumpableType stage Type.Type where
    refer_type t = do
        adt_arena <- get_adt_arena
        type_synonym_arena <- get_type_synonym_arena
        quant_var_arena <- get_quant_var_arena
        pure (Type.PP.refer_type adt_arena type_synonym_arena quant_var_arena t)

class DumpableIdentifier stage i where
    refer_iden :: i -> IRReader stage PP.Token

instance DumpableIdentifier stage a => DumpableIdentifier stage (Located a) where
    refer_iden = refer_iden . unlocate
instance DumpableIdentifier stage t => DumpableIdentifier stage (Maybe t) where -- TODO: remove this
    refer_iden (Just t) = refer_iden t
    refer_iden Nothing = pure $ PP.String "<name resolution error>"

instance DumpableIdentifier stage Text where
    refer_iden = pure . PP.String

-- TODO: remove these
-- instance DumpableType stage t => DumpableIdentifier stage (SIR.DeclRef t) where
--     refer_iden = refer_decl
-- instance DumpableIdentifier stage SIR.ValueRef where
--     refer_iden = refer_bv
-- instance DumpableIdentifier stage Type.ADT.VariantIndex where
--     refer_iden = refer_adt_variant

-- TODO: figure out how to overload this for if resolved is not ()
refer_split_iden :: (DumpableConstraints stage) => SIR.SplitIdentifier id_name stage -> IRReader stage PP.Token
refer_split_iden (SIR.SplitIdentifier'Get _ texpr next) = type_expr texpr >>= \ texpr -> pure (PP.List [texpr, "::", PP.String $ unlocate next])
refer_split_iden (SIR.SplitIdentifier'Single _ _ name) = refer_iden name

-- TODO: figure out how to overload this for if resolved is not ()
refer_split_iden_and_resolved :: (DumpableConstraints stage) => SIR.SplitIdentifier id_name stage -> resolved -> IRReader stage PP.Token
refer_split_iden_and_resolved (SIR.SplitIdentifier'Get _ texpr next) _ = type_expr texpr >>= \ texpr -> pure (PP.List [texpr, "::", PP.String $ unlocate next])
refer_split_iden_and_resolved (SIR.SplitIdentifier'Single _ _ name) _ = refer_iden name

-- TODO: dump type info too

quant_var :: Type.QuantVarKey -> IRReader stage PP.Token
quant_var k = get_quant_var k >>= \ (Type.QuantVar (Located _ name)) -> pure $ PP.String name

type_expr :: DumpableConstraints stage => SIR.TypeExpr stage -> IRReader stage PP.Token
type_expr = PP.Precedence.pp_precedence_m levels PP.Precedence.parenthesize
    where
        levels (SIR.TypeExpr'Forall _ _ _ vars ty) = (1, \ cur _ -> mapM quant_var vars >>= \ vars -> cur ty >>= \ ty -> pure (PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent $ toList vars, " ", ty]))
        levels (SIR.TypeExpr'Function _ _ arg res) = (2, \ cur next -> next arg >>= \ arg -> cur res >>= \ res -> pure (PP.List [arg, " -> ", res]))
        levels (SIR.TypeExpr'Apply _ _ ty arg) = (3, \ cur _ -> cur ty >>= \ ty -> type_expr arg >>= \ arg -> pure (PP.List [ty, "#(", arg, ")"]))
        levels (SIR.TypeExpr'Get _ _ resolved parent name) = (3, \ cur _ -> cur parent >>= \ parent -> pure (PP.List [parent, "::", PP.String $ unlocate name]))
        levels (SIR.TypeExpr'Refer _ _ _ _ iden) = (4, \ _ _ -> refer_iden iden)
        levels (SIR.TypeExpr'Tuple _ _ a b) = (4, \ _ _ -> type_expr a >>= \ a -> type_expr b >>= \ b -> pure (PP.parenthesized_comma_list PP.Inconsistent [a, b]))
        levels (SIR.TypeExpr'Hole _ _ _ hid) = (4, \ _ _ -> pure $ PP.List ["?", PP.String $ unlocate hid])
        levels (SIR.TypeExpr'Wild _ _) = (4, \ _ _ -> pure $ PP.String "_")
        levels (SIR.TypeExpr'Poison _ _) = (4, \ _ _ -> pure $ PP.String "poison")

expr :: DumpableConstraints stage => SIR.Expr stage -> IRReader stage PP.Token
expr = PP.Precedence.pp_precedence_m levels PP.Precedence.parenthesize
    where
        levels (SIR.Expr'BinaryOps _ _ _ _ first ops) =
            (0, \ _ next ->
                next first >>= \ first ->
                mapM
                    (\ (_, op_iden, rhs) ->
                        refer_split_iden op_iden >>= \ op ->
                        next rhs >>= \ rhs ->
                        pure (PP.List [op, " ", rhs]))
                    ops >>= \ ops ->
                pure (PP.List [first, PP.Block PP.Inconsistent Nothing (Just " ") Nothing ops]))
        levels (SIR.Expr'Call _ _ _ callee arg) = (1, \ cur _ -> cur callee >>= \ callee -> expr arg >>= \ arg -> pure (PP.FirstOnLineIfMultiline $ PP.List [callee, "(", arg, ")"]))
        levels (SIR.Expr'TypeApply _ _ _ e (arg, _)) = (1, \ cur _ -> cur e >>= \ e -> type_expr arg >>= \ arg -> pure (PP.List [e, "#(", arg, ")"]))
        levels (SIR.Expr'Refer _ _ _ iden) = (2, \ _ _ -> PP.FirstOnLineIfMultiline <$> refer_split_iden iden)
        levels (SIR.Expr'Hole _ _ _ hid) = (2, \ _ _ -> pure $ PP.List ["?", PP.String $ unlocate hid])
        levels (SIR.Expr'Poison _ _ _) = (2, \ _ _ -> pure $ PP.String "poison")
        levels (SIR.Expr'Char _ _ _ c) = (2, \ _ _ -> pure $ PP.FirstOnLineIfMultiline $ PP.String $ show c)
        levels (SIR.Expr'String _ _ _ s) = (2, \ _ _ -> pure $ PP.FirstOnLineIfMultiline $ PP.String $ show s)
        levels (SIR.Expr'Int _ _ _ i) = (2, \ _ _ -> pure $ PP.FirstOnLineIfMultiline $ PP.String $ show i)
        levels (SIR.Expr'Float _ _ _ (n :% d)) = (2, \ _ _ -> pure $ PP.FirstOnLineIfMultiline $ PP.String $ "(" <> show n <> "/" <> show d <> ")")
        levels (SIR.Expr'Bool _ _ _ b) = (2, \ _ _ -> pure $ PP.FirstOnLineIfMultiline $ PP.String $ if b then "true" else "false")
        levels (SIR.Expr'Tuple _ _ _ a b) = (2, \ _ _ -> expr a >>= \ a -> expr b >>= \ b -> pure (PP.parenthesized_comma_list PP.Inconsistent [a, b]))
        levels (SIR.Expr'Lambda _ _ _ param body) = (2, \ _ _ -> PP.FirstOnLineIfMultiline <$> (pattern param >>= \ param -> expr body >>= \ body -> pure (PP.List ["\\ ", param, " -> ", body]))) -- TODO: decide if this should be \ (x) -> or \ x ->
        levels (SIR.Expr'Let _ _ _ _ bindings adts type_synonyms body) = (2, \ _ _ -> pp_let "let" bindings adts type_synonyms body)
        levels (SIR.Expr'LetRec _ _ _ _ bindings adts type_synonyms body) = (2, \ _ _ -> pp_let "letrec" bindings adts type_synonyms body)
        levels (SIR.Expr'If _ _ _ _ cond t f) = (2, \ _ _ -> expr cond >>= \ cond -> expr t >>= \ t -> expr f >>= \ f -> pure (PP.FirstOnLineIfMultiline $ PP.List ["if ", cond, " then ", t, " else ", f]))
        levels (SIR.Expr'Match _ _ _ _ e arms) = (2, \ _ _ -> expr e >>= \ e -> mapM (\ (_, p, e) -> pattern p >>= \ p -> expr e >>= \ e -> pure (PP.List [p, " -> ", e, ";"])) arms >>= \ arms -> pure (PP.List ["match ", e, " ", PP.braced_block arms]))
        levels (SIR.Expr'TypeAnnotation _ _ _ (ty, _) e) = (2, \ _ _ -> type_expr ty >>= \ ty -> expr e >>= \ e -> pure (PP.List [":", ty, ": ", e]))
        levels (SIR.Expr'Forall _ _ _ _ tys e) = (2, \ _ _ -> mapM quant_var tys >>= \ tys -> expr e >>= \ e -> pure (PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent $ toList tys, " ", e]))
pp_let :: DumpableConstraints stage => Text -> [SIR.Binding stage] -> [Type.ADTKey] -> [Type.TypeSynonymKey] -> SIR.Expr stage -> IRReader stage PP.Token
pp_let let_kw bindings adts type_synonyms body = do
    sir <- ask
    bindings <- mapM define_binding bindings
    quant_var_arena <- get_quant_var_arena
    adts <- mapM (\ k -> get_adt k >>= \ adt -> pure (Type.PP.define_adt quant_var_arena (\ (ty, _) -> runReader (type_expr ty) sir) adt)) adts
    type_synonyms <- mapM (\ k -> get_type_syn k >>= \ ts -> pure (Type.PP.define_type_synonym (\ (ty, _) -> runReader (type_expr ty) sir) ts)) type_synonyms
    body <- expr body
    let all_decls = adts ++ type_synonyms ++ bindings
    pure
        $ case all_decls of
            [decl] -> PP.FirstOnLineIfMultiline $ PP.List [PP.String let_kw, " ", decl, "\n", body]
            _ -> PP.FirstOnLineIfMultiline $ PP.List [PP.String let_kw, " ", PP.braced_block all_decls, "\n", body]
pattern :: DumpableConstraints stage => SIR.Pattern stage -> IRReader stage PP.Token
pattern (SIR.Pattern'Variable _ _ var_key) = refer_var var_key
pattern (SIR.Pattern'Wildcard _ _) = pure $ PP.String "_"
pattern (SIR.Pattern'Tuple _ _ a b) = pattern a >>= \ a -> pattern b >>= \ b -> pure (PP.parenthesized_comma_list PP.Inconsistent [a, b])
pattern (SIR.Pattern'Named _ _ _ var_key subpat) = refer_var (unlocate var_key) >>= \ var_key -> pattern subpat >>= \ subpat -> pure (PP.List ["@", var_key, " ", subpat])
pattern (SIR.Pattern'AnonADTVariant _ _ _ variant_iden fields) = refer_split_iden variant_iden >>= \ variant -> mapM pattern fields >>= \ fields -> pure (PP.List [variant, PP.parenthesized_comma_list PP.Inconsistent fields])
pattern (SIR.Pattern'NamedADTVariant _ _ _ variant_iden fields) = refer_split_iden variant_iden >>= \ variant -> mapM (\ (field_name, field_pat) -> pattern field_pat >>= \ field_pat -> pure (PP.List [PP.String $ unlocate field_name, " = ", field_pat, ";"])) fields >>= \ fields -> pure (PP.List [variant, PP.braced_block fields])
pattern (SIR.Pattern'Poison _ _) = pure $ PP.String "poison"
