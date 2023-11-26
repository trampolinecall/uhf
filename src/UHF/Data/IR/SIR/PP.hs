{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module UHF.Data.IR.SIR.PP (dump_main_module) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.PP as PP
import qualified UHF.PP.Precedence as PP.Precedence

import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.PP as Type.PP
import qualified UHF.Data.IR.ID as ID

import UHF.IO.Located (Located (Located, unlocate))

type IRReader stage = Reader (SIR.SIR stage)

type DumpableConstraints stage =
    ( DumpableIdentifier stage (SIR.DIdenStart stage)
    , DumpableIdentifier stage (SIR.VIdenStart stage)
    , DumpableIdentifier stage (SIR.PIdenStart stage)
    , DumpableIdentifier stage (SIR.SplitIdentifier stage (SIR.VIdenStart stage), SIR.VIdenResolved stage)
    , DumpableIdentifier stage (SIR.SplitIdentifier stage (SIR.PIdenStart stage), SIR.PIdenResolved stage)
    )

dump_main_module :: DumpableConstraints stage => SIR.SIR stage -> Text
dump_main_module ir@(SIR.SIR _ modules _ _ _ _ mod) = PP.render $ runReader (define_module $ Arena.get modules mod) ir

get_adt_arena :: IRReader stage (Arena.Arena (Type.ADT (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage)) Type.ADTKey)
get_adt_arena = reader (\ (SIR.SIR _ _ adts _ _ _ _) -> adts)
get_type_synonym_arena :: IRReader stage (Arena.Arena (Type.TypeSynonym (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage)) Type.TypeSynonymKey)
get_type_synonym_arena = reader (\ (SIR.SIR _ _ _ syns _ _ _) -> syns)
get_type_var_arena :: IRReader stage (Arena.Arena Type.Var Type.TypeVarKey)
get_type_var_arena = reader (\ (SIR.SIR _ _ _ _ vars _ _) -> vars)

get_bv :: SIR.BoundValueKey -> IRReader stage (SIR.BoundValue stage)
get_bv k = reader (\ (SIR.SIR _ _ _ _ _ bvs _) -> Arena.get bvs k)
get_decl :: SIR.DeclKey -> IRReader stage SIR.Decl
get_decl k = reader (\ (SIR.SIR decls _ _ _ _ _ _) -> Arena.get decls k)
get_module :: SIR.ModuleKey -> IRReader stage (SIR.Module stage)
get_module k = reader (\ (SIR.SIR _ modules _ _ _ _ _) -> Arena.get modules k)
get_adt :: Type.ADTKey -> IRReader stage (Type.ADT (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage))
get_adt k = reader (\ (SIR.SIR _ _ adts _ _ _ _) -> Arena.get adts k)
get_type_syn :: Type.TypeSynonymKey -> IRReader stage (Type.TypeSynonym (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage))
get_type_syn k = reader (\ (SIR.SIR _ _ _ syns _ _ _) -> Arena.get syns k)
get_type_var :: Type.TypeVarKey -> IRReader stage Type.Var
get_type_var k = reader (\ (SIR.SIR _ _ _ _ type_vars _ _) -> Arena.get type_vars k)

define_module :: DumpableConstraints stage => SIR.Module stage -> IRReader stage PP.Token
define_module (SIR.Module _ bindings adts type_synonyms) =
    ask >>= \ sir ->
    mapM (\ k -> get_adt k >>= \ adt -> pure (Type.PP.define_adt adt)) adts >>= \ adts_defined ->
    mapM (\ k -> get_type_syn k >>= \ ts -> pure (Type.PP.define_type_synonym (\ (ty, _) -> runReader (type_expr ty) sir) ts)) type_synonyms >>= \ type_synonyms_defined ->
    mapM define_binding bindings >>= \ bindings_defined ->
    pure (PP.flat_block $ adts_defined <> type_synonyms_defined <> bindings_defined)

define_binding :: DumpableConstraints stage => SIR.Binding stage -> IRReader stage PP.Token
define_binding (SIR.Binding pat _ init) = pattern pat >>= \ pat -> expr init >>= \ init -> pure $ PP.List [pat, " = ", init, ";"]
define_binding (SIR.Binding'ADTVariant _ bvk _ variant_index@(Type.ADTVariantIndex adt_key _)) =
    Type.PP.refer_adt <$> get_adt adt_key >>= \ adt_refer ->
    Type.get_adt_variant <$> get_adt_arena <*> pure variant_index >>= \ variant ->
    let variant_name = unlocate $ Type.variant_name variant
    in refer_bv bvk >>= \ bvk ->
    pure $ PP.List [bvk, " = <constructor for ", adt_refer, " ", PP.String variant_name, ">;"]

class DumpableIdentifier stage i where
    refer_iden :: i -> IRReader stage PP.Token

refer_bv :: SIR.BoundValueKey -> IRReader stage PP.Token
refer_bv k = get_bv k >>= \case
    SIR.BoundValue id _ _ -> pure $ PP.String (ID.stringify id)
    SIR.BoundValue'ADTVariant id _ _ _ _ -> pure $ PP.String (ID.stringify id)

refer_decl :: SIR.DeclKey -> IRReader stage PP.Token
refer_decl k = get_decl k >>= \case
    SIR.Decl'Module m ->
        get_module m >>= \ (SIR.Module id _ _ _) ->
        pure (PP.String $ ID.stringify id)
    SIR.Decl'Type ty ->
        get_adt_arena >>= \ adt_arena ->
        get_type_synonym_arena >>= \ type_synonym_arena ->
        get_type_var_arena >>= \ type_var_arena ->
        pure (Type.PP.refer_type absurd adt_arena type_synonym_arena type_var_arena ty)

instance DumpableIdentifier stage a => DumpableIdentifier stage (Located a) where
    refer_iden = refer_iden . unlocate
instance DumpableIdentifier stage t => DumpableIdentifier stage (Maybe t) where -- TODO: remove this
    refer_iden (Just t) = refer_iden t
    refer_iden Nothing = pure $ PP.String "<name resolution error>"

instance (DumpableConstraints stage, DumpableIdentifier stage start) => DumpableIdentifier stage (SIR.SplitIdentifier stage start) where
    refer_iden (SIR.SplitIdentifier'Get texpr next) = type_expr texpr >>= \ texpr -> pure (PP.List [texpr, "::", PP.String $ unlocate next])
    refer_iden (SIR.SplitIdentifier'Single start) = refer_iden start

instance (DumpableConstraints stage, DumpableIdentifier stage start) => DumpableIdentifier stage (SIR.SplitIdentifier stage start, resolved) where
    refer_iden (a, _) = refer_iden a -- TODO: figure out how to use resolved but only if it is not ()

instance DumpableIdentifier stage Text where
    refer_iden = pure . PP.String

instance DumpableIdentifier stage SIR.DeclKey where
    refer_iden = refer_decl
instance DumpableIdentifier stage SIR.BoundValueKey where
    refer_iden = refer_bv
instance DumpableIdentifier stage Type.ADTVariantIndex where
    refer_iden variant_index@(Type.ADTVariantIndex adt_key _) =
        Type.PP.refer_adt <$> get_adt adt_key >>= \ adt_referred ->
        Type.get_adt_variant <$> get_adt_arena <*> pure variant_index >>= \ variant ->
        let variant_name = unlocate $ Type.variant_name variant
        in pure $ PP.List [adt_referred, "::", PP.String variant_name]

-- TODO: dump type info too

type_var :: Type.TypeVarKey -> IRReader stage PP.Token
type_var k = get_type_var k >>= \ (Type.Var (Located _ name)) -> pure $ PP.String name

type_expr :: DumpableConstraints stage => SIR.TypeExpr stage -> IRReader stage PP.Token
type_expr = PP.Precedence.pp_precedence_m levels PP.Precedence.parenthesize
    where
        levels (SIR.TypeExpr'Forall _ _ vars ty) = (1, \ cur _ -> mapM type_var vars >>= \ vars -> cur ty >>= \ ty -> pure (PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent $ toList vars, " ", ty]))
        levels (SIR.TypeExpr'Function _ _ arg res) = (2, \ cur next -> next arg >>= \ arg -> cur res >>= \ res -> pure (PP.List [arg, " -> ", res]))
        levels (SIR.TypeExpr'Apply _ _ ty arg) = (3, \ cur _ -> cur ty >>= \ ty -> type_expr arg >>= \ arg -> pure (PP.List [ty, "#(", arg, ")"]))
        levels (SIR.TypeExpr'Get _ _ parent name) = (3, \ cur _ -> cur parent >>= \ parent -> pure (PP.List [parent, "::", PP.String $ unlocate name]))
        levels (SIR.TypeExpr'Refer _ _ iden) = (4, \ _ _ -> refer_iden iden)
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
                    (\ (_, op_split_iden, op_resolved, rhs) ->
                        refer_iden (op_split_iden, op_resolved) >>= \ op ->
                        next rhs >>= \ rhs ->
                        pure (PP.List [op, " ", rhs]))
                    ops >>= \ ops ->
                pure (PP.List ["(", first, PP.Block PP.Inconsistent Nothing (Just " ") Nothing ops, ")"]))

        levels (SIR.Expr'Call _ _ _ callee arg) = (1, \ cur _ -> cur callee >>= \ callee -> expr arg >>= \ arg -> pure (PP.FirstOnLineIfMultiline $ PP.List [callee, "(", arg, ")"]))
        levels (SIR.Expr'TypeApply _ _ _ e (arg, _)) = (1, \ cur _ -> cur e >>= \ e -> type_expr arg >>= \ arg -> pure (PP.List [e, "#(", arg, ")"]))

        levels (SIR.Expr'Identifier _ _ _ split resolved) = (2, \ _ _ -> PP.FirstOnLineIfMultiline <$> refer_iden (split, resolved))
        levels (SIR.Expr'Hole _ _ _ hid) = (2, \ _ _ -> pure $ PP.List ["?", PP.String $ unlocate hid])
        levels (SIR.Expr'Poison _ _ _) = (2, \ _ _ -> pure $ PP.String "poison")
        levels (SIR.Expr'Char _ _ _ c) = (2, \ _ _ -> pure $ PP.FirstOnLineIfMultiline $ PP.String $ show c)
        levels (SIR.Expr'String _ _ _ s) = (2, \ _ _ -> pure $ PP.FirstOnLineIfMultiline $ PP.String $ show s)
        levels (SIR.Expr'Int _ _ _ i) = (2, \ _ _ -> pure $ PP.FirstOnLineIfMultiline $ PP.String $ show i)
        levels (SIR.Expr'Float _ _ _ (n :% d)) = (2, \ _ _ -> pure $ PP.FirstOnLineIfMultiline $ PP.String $ "(" <> show n <> "/" <> show d <> ")")
        levels (SIR.Expr'Bool _ _ _ b) = (2, \ _ _ -> pure $ PP.FirstOnLineIfMultiline $ PP.String $ if b then "true" else "false")
        levels (SIR.Expr'Tuple _ _ _ a b) = (2, \ _ _ -> expr a >>= \ a -> expr b >>= \ b -> pure (PP.parenthesized_comma_list PP.Inconsistent [a, b]))
        levels (SIR.Expr'Lambda _ _ _ param body) = (2, \ _ _ -> PP.FirstOnLineIfMultiline <$> (pattern param >>= \ param -> expr body >>= \ body -> pure (PP.List ["\\ ", param, " -> ", body]))) -- TODO: decide if this should be \ (x) -> or \ x ->

        levels (SIR.Expr'Let _ _ _ bindings body) = (2, \ _ _ -> pp_let "let" bindings body)
        levels (SIR.Expr'LetRec _ _ _ bindings body) = (2, \ _ _ -> pp_let "letrec" bindings body)

        levels (SIR.Expr'If _ _ _ _ cond t f) = (2, \ _ _ -> expr cond >>= \ cond -> expr t >>= \ t -> expr f >>= \ f -> pure (PP.FirstOnLineIfMultiline $ PP.List ["if ", cond, " then ", t, " else ", f]))
        levels (SIR.Expr'Match _ _ _ _ e arms) = (2, \ _ _ -> expr e >>= \ e -> mapM (\ (p, e) -> pattern p >>= \ p -> expr e >>= \ e -> pure (PP.List [p, " -> ", e, ";"])) arms >>= \ arms -> pure (PP.List ["match ", e, " ", PP.braced_block arms]))

        levels (SIR.Expr'TypeAnnotation _ _ _ (ty, _) e) = (2, \ _ _ -> type_expr ty >>= \ ty -> expr e >>= \ e -> pure (PP.List [":", ty, ": ", e]))

        levels (SIR.Expr'Forall _ _ _ tys e) = (2, \ _ _ -> mapM type_var tys >>= \ tys -> expr e >>= \ e -> pure (PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent $ toList tys, " ", e]))

pp_let :: DumpableConstraints stage => Text -> [SIR.Binding stage] -> SIR.Expr stage -> IRReader stage PP.Token
pp_let let_kw [binding] body = define_binding binding >>= \ binding -> expr body >>= \ body -> pure (PP.FirstOnLineIfMultiline $ PP.List [PP.String let_kw, " ", binding, "\n", body])
pp_let let_kw bindings body = mapM define_binding bindings >>= \ bindings -> expr body >>= \ body -> pure (PP.FirstOnLineIfMultiline $ PP.List [PP.String let_kw, " ", PP.braced_block bindings, "\n", body])

pattern :: DumpableConstraints stage => SIR.Pattern stage -> IRReader stage PP.Token
pattern (SIR.Pattern'Identifier _ _ bvk) = refer_iden bvk
pattern (SIR.Pattern'Wildcard _ _) = pure $ PP.String "_"
pattern (SIR.Pattern'Tuple _ _ a b) = pattern a >>= \ a -> pattern b >>= \ b -> pure (PP.parenthesized_comma_list PP.Inconsistent [a, b])
pattern (SIR.Pattern'Named _ _ _ bvk subpat) = refer_iden (unlocate bvk) >>= \ bvk -> pattern subpat >>= \ subpat -> pure (PP.List ["@", bvk, " ", subpat])
pattern (SIR.Pattern'AnonADTVariant _ _ variant_split_iden variant_resolved_iden _ fields) = refer_iden (variant_split_iden, variant_resolved_iden) >>= \ variant -> mapM pattern fields >>= \ fields -> pure (PP.List [variant, PP.parenthesized_comma_list PP.Inconsistent fields])
pattern (SIR.Pattern'NamedADTVariant _ _ variant_split_iden variant_resolved_iden _ fields) = refer_iden (variant_split_iden, variant_resolved_iden) >>= \ variant -> mapM (\ (field_name, field_pat) -> pattern field_pat >>= \ field_pat -> pure (PP.List [PP.String $ unlocate field_name, " = ", field_pat, ";"])) fields >>= \ fields -> pure (PP.List [variant, PP.braced_block fields])
pattern (SIR.Pattern'Poison _ _) = pure $ PP.String "poison"
