{-# LANGUAGE FlexibleInstances #-}

module UHF.Data.IR.SIR.PP (dump_main_module) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.PP as PP

import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.PP as Type.PP
import qualified UHF.Data.IR.ID as ID

import UHF.IO.Located (Located (unlocate))

import qualified Data.Text as Text

type IRReader d_iden v_iden type_info binary_ops_allowed = Reader (SIR.SIR d_iden v_iden type_info binary_ops_allowed)

dump_main_module :: (DumpableIdentifier d_iden, DumpableIdentifier v_iden) => SIR.SIR d_iden v_iden type_info binary_ops_allowed -> Text
dump_main_module ir@(SIR.SIR decls _ _ _ _ mod) = PP.render $ runReader (define_decl $ Arena.get decls mod) ir

get_adt_arena :: IRReader d_iden v_iden type_info binary_ops_allowed (Arena.Arena (Type.ADT (SIR.TypeExpr d_iden type_info)) Type.ADTKey)
get_adt_arena = reader (\ (SIR.SIR _ adts _ _ _ _) -> adts)
get_type_synonym_arena :: IRReader d_iden v_iden type_info binary_ops_allowed (Arena.Arena (Type.TypeSynonym (SIR.TypeExpr d_iden type_info)) Type.TypeSynonymKey)
get_type_synonym_arena = reader (\ (SIR.SIR _ _ syns _ _ _) -> syns)
get_type_var_arena :: IRReader d_iden v_iden type_info binary_ops_allowed (Arena.Arena Type.Var Type.TypeVarKey)
get_type_var_arena = reader (\ (SIR.SIR _ _ _ vars _ _) -> vars)

get_bv :: SIR.BoundValueKey -> IRReader d_iden v_iden type_info binary_ops_allowed (SIR.BoundValue type_info)
get_bv k = reader (\ (SIR.SIR _ _ _ _ bvs _) -> Arena.get bvs k)
get_decl :: SIR.DeclKey -> IRReader d_iden v_iden type_info binary_ops_allowed (SIR.Decl d_iden v_iden type_info binary_ops_allowed)
get_decl k = reader (\ (SIR.SIR decls _ _ _ _ _) -> Arena.get decls k)
get_adt :: Type.ADTKey -> IRReader d_iden v_iden type_info binary_ops_allowed (Type.ADT (SIR.TypeExpr d_iden type_info))
get_adt k = reader (\ (SIR.SIR _ adts _ _ _ _) -> Arena.get adts k)
get_type_syn :: Type.TypeSynonymKey -> IRReader d_iden v_iden type_info binary_ops_allowed (Type.TypeSynonym (SIR.TypeExpr d_iden type_info))
get_type_syn k = reader (\ (SIR.SIR _ _ syns _ _ _) -> Arena.get syns k)
get_type_var :: Type.TypeVarKey -> IRReader d_iden v_iden type_info binary_ops_allowed Type.Var
get_type_var k = reader (\ (SIR.SIR _ _ _ type_vars _ _) -> Arena.get type_vars k)

define_decl :: (DumpableIdentifier d_iden, DumpableIdentifier v_iden) => SIR.Decl d_iden v_iden type_info binary_ops_allowed -> IRReader d_iden v_iden type_info binary_ops_allowed PP.Token
define_decl (SIR.Decl'Module _ _ bindings adts type_synonyms) =
    ask >>= \ sir ->
    mapM (\ k -> get_adt k >>= \ adt -> pure (Type.PP.define_adt adt)) adts >>= \ adts_defined ->
    mapM (\ k -> get_type_syn k >>= \ ts -> pure (Type.PP.define_type_synonym (\ ty -> runReader (type_expr ty) sir) ts)) type_synonyms >>= \ type_synonyms_defined ->
    mapM define_binding bindings >>= \ bindings_defined ->
    pure (PP.flat_block $ adts_defined <> type_synonyms_defined <> bindings_defined)
define_decl (SIR.Decl'Type _) = pure (PP.List [])

define_binding :: (DumpableIdentifier d_iden, DumpableIdentifier v_iden) => SIR.Binding d_iden v_iden type_info binary_ops_allowed -> IRReader d_iden v_iden type_info binary_ops_allowed PP.Token
define_binding (SIR.Binding pat _ init) = pattern pat >>= \ pat -> expr init >>= \ init -> pure $ PP.List [pat, " = ", init, ";"]
define_binding (SIR.Binding'ADTVariant bvk variant_index@(Type.ADTVariantIndex adt_key _)) =
    Type.PP.refer_adt <$> get_adt adt_key >>= \ adt_refer ->
    Type.get_adt_variant <$> get_adt_arena <*> pure variant_index >>= \ variant ->
    let variant_name = Type.variant_name variant
    in refer_bv bvk >>= \ bvk ->
    pure $ PP.List [bvk, " = <constructor for ", adt_refer, " ", PP.String variant_name, ">;"]

class DumpableIdentifier i where
    refer_iden :: i -> IRReader d_iden v_iden type_info binary_ops_allowed PP.Token

refer_bv :: SIR.BoundValueKey -> IRReader d_iden v_iden type_info binary_ops_allowed PP.Token
refer_bv k = get_bv k >>= \case
    SIR.BoundValue id _ _ -> pure $ PP.String (ID.stringify id)
    SIR.BoundValue'ADTVariant id _ _ _ -> pure $ PP.String (ID.stringify id)

refer_decl :: SIR.DeclKey -> IRReader d_iden v_iden type_info binary_ops_allowed PP.Token
refer_decl k = get_decl k >>= \case
    SIR.Decl'Module id _ _ _ _ -> pure $ PP.String $ ID.stringify id
    SIR.Decl'Type ty ->
        get_adt_arena >>= \ adt_arena ->
        get_type_synonym_arena >>= \ type_synonym_arena ->
        get_type_var_arena >>= \ type_var_arena ->
        pure (Type.PP.refer_type absurd adt_arena type_synonym_arena type_var_arena ty)

put_iden_list_of_text :: [Located Text] -> IRReader d_iden v_iden type_info binary_ops_allowed PP.Token
put_iden_list_of_text = pure . PP.String . Text.intercalate "::" . map unlocate

instance DumpableIdentifier (SIR.NameContext, [Located Text]) where
    refer_iden (_, segments) = put_iden_list_of_text segments
instance DumpableIdentifier (Located (Maybe SIR.BoundValueKey)) where -- TODO: remove this
    refer_iden k = case unlocate k of
        Just k -> refer_iden k
        Nothing -> pure $ PP.String "<name resolution error>"
instance DumpableIdentifier SIR.BoundValueKey where
    refer_iden = refer_bv
instance DumpableIdentifier (Maybe SIR.DeclKey) where -- TODO: remove this
    refer_iden (Just k) = refer_decl k
    refer_iden Nothing = pure $ PP.String "<name resolution error>"

-- TODO: dump type info too

type_var :: Type.TypeVarKey -> IRReader d_iden v_iden type_info binary_ops_allowed PP.Token
type_var k = get_type_var k >>= \ (Type.Var name) -> pure $ PP.String name

-- TODO: precedence
type_expr :: DumpableIdentifier d_iden => SIR.TypeExpr d_iden type_info -> IRReader d_iden v_iden type_info binary_ops_allowed PP.Token
type_expr (SIR.TypeExpr'Identifier _ _ iden) = refer_iden iden
type_expr (SIR.TypeExpr'Tuple _ a b) = type_expr a >>= \ a -> type_expr b >>= \ b -> pure (PP.parenthesized_comma_list [a, b])
type_expr (SIR.TypeExpr'Hole _ _ hid) = put_iden_list_of_text (unlocate hid) >>= \ hid -> pure (PP.List ["?", hid])
type_expr (SIR.TypeExpr'Forall _ vars ty) = mapM type_var vars >>= \ vars -> type_expr ty >>= \ ty -> pure (PP.List ["#", PP.parenthesized_comma_list $ toList vars, " ", ty])
type_expr (SIR.TypeExpr'Apply _ _ ty arg) = type_expr ty >>= \ ty -> type_expr arg >>= \ arg -> pure (PP.List [ty, "#(", arg, ")"])
type_expr (SIR.TypeExpr'Wild _ _) = pure $ PP.String "_"
type_expr (SIR.TypeExpr'Poison _ _) = pure $ PP.String "poison"

-- TODO: deal with precedence
expr :: (DumpableIdentifier d_iden, DumpableIdentifier v_iden) => SIR.Expr d_iden v_iden type_info binary_ops_allowed -> IRReader d_iden v_iden type_info binary_ops_allowed PP.Token
expr (SIR.Expr'Identifier _ _ _ i) = PP.FirstOnLineIfMultiline <$> refer_iden i
expr (SIR.Expr'Char _ _ _ c) = pure $ PP.FirstOnLineIfMultiline $ PP.String $ show c
expr (SIR.Expr'String _ _ _ s) = pure $ PP.FirstOnLineIfMultiline $ PP.String $ show s
expr (SIR.Expr'Int _ _ _ i) = pure $ PP.FirstOnLineIfMultiline $ PP.String $ show i
expr (SIR.Expr'Float _ _ _ (n :% d)) = pure $ PP.FirstOnLineIfMultiline $ PP.String $ "(" <> show n <> "/" <> show d <> ")"
expr (SIR.Expr'Bool _ _ _ b) = pure $ PP.FirstOnLineIfMultiline $ PP.String $ if b then "true" else "false"
expr (SIR.Expr'Tuple _ _ _ a b) = expr a >>= \ a -> expr b >>= \ b -> pure (PP.parenthesized_comma_list [a, b])
expr (SIR.Expr'Lambda _ _ _ param body) = PP.FirstOnLineIfMultiline <$> (pattern param >>= \ param -> expr body >>= \ body -> pure (PP.List ["\\ ", param, " -> ", body])) -- TODO: decide if this should be \ (x) -> or \ x ->
expr (SIR.Expr'Let _ _ _ [binding] body) = define_binding binding >>= \ binding -> expr body >>= \ body -> pure (PP.FirstOnLineIfMultiline $ PP.List ["let ", binding, "\n", body])
expr (SIR.Expr'Let _ _ _ bindings body) = mapM define_binding bindings >>= \ bindings -> expr body >>= \ body -> pure (PP.FirstOnLineIfMultiline $ PP.List ["let", PP.braced_comma_list bindings, "\n", body])
expr (SIR.Expr'BinaryOps _ _ _ _ first ops) = expr first >>= \ first -> mapM (\ (op, rhs) -> refer_iden op >>= \ op -> expr rhs >>= \ rhs -> pure (PP.List [op, " ", rhs])) ops >>= \ ops -> pure (PP.List ["(", first, PP.indented_block ops, ")"])
expr (SIR.Expr'Call _ _ _ callee arg) = expr callee >>= \ callee -> expr arg >>= \ arg -> pure (PP.FirstOnLineIfMultiline $ PP.List [callee, "(", arg, ")"])
expr (SIR.Expr'If _ _ _ _ cond t f) = expr cond >>= \ cond -> expr t >>= \ t -> expr f >>= \ f -> pure (PP.FirstOnLineIfMultiline $ PP.List ["if ", cond, " then ", t, " else ", f])
expr (SIR.Expr'Case _ _ _ _ e arms) = expr e >>= \ e -> mapM (\ (p, e) -> pattern p >>= \ p -> expr e >>= \ e -> pure (PP.List [p, " -> ", e, ";"])) arms >>= \ arms -> pure (PP.List ["case ", e, " ", PP.braced_block arms])
expr (SIR.Expr'TypeAnnotation _ _ _ ty e) = type_expr ty >>= \ ty -> expr e >>= \ e -> pure (PP.List [":", ty, ": ", e])
expr (SIR.Expr'Hole _ _ _ hid) = put_iden_list_of_text (unlocate hid) >>= \ hid -> pure (PP.List ["?", hid])
expr (SIR.Expr'Forall _ _ _ tys e) = mapM type_var tys >>= \ tys -> expr e >>= \ e -> pure (PP.List ["#", PP.parenthesized_comma_list $ toList tys, " ", e])
expr (SIR.Expr'TypeApply _ _ _ e arg) = expr e >>= \ e -> type_expr arg >>= \ arg -> pure (PP.List [e, "#(", arg, ")"])
expr (SIR.Expr'Poison _ _ _) = pure $ PP.String "poison"

pattern :: SIR.Pattern type_info -> IRReader d_iden v_iden type_info binary_ops_allowed PP.Token
pattern (SIR.Pattern'Identifier _ _ bvk) = refer_iden bvk
pattern (SIR.Pattern'Wildcard _ _) = pure $ PP.String "_"
pattern (SIR.Pattern'Tuple _ _ a b) = pattern a >>= \ a -> pattern b >>= \ b -> pure (PP.parenthesized_comma_list [a, b])
pattern (SIR.Pattern'Named _ _ _ bvk subpat) = refer_iden (unlocate bvk) >>= \ bvk -> pattern subpat >>= \ subpat -> pure (PP.List ["@", bvk, " ", subpat])
pattern (SIR.Pattern'Poison _ _) = pure $ PP.String "poison"
