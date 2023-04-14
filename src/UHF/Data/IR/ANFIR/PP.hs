{-# LANGUAGE FlexibleInstances #-}

module UHF.Data.IR.ANFIR.PP (dump_main_module) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.PP as PP

import qualified UHF.Data.IR.ANFIR as ANFIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.PP as Type.PP
import qualified UHF.Data.IR.ID as ID

-- TODO: dump types too

type IRReader captures ty poison_allowed = Reader (ANFIR.ANFIR captures ty poison_allowed)

get_adt_arena :: IRReader captures ty poison_allowed (Arena.Arena (Type.ADT ty) Type.ADTKey)
get_adt_arena = reader (\ (ANFIR.ANFIR _ adts _ _ _ _ _) -> adts)
get_type_synonym_arena :: IRReader captures ty poison_allowed (Arena.Arena (Type.TypeSynonym ty) Type.TypeSynonymKey)
get_type_synonym_arena = reader (\ (ANFIR.ANFIR _ _ syns _ _ _ _) -> syns)
get_type_var_arena :: IRReader captures ty poison_allowed (Arena.Arena Type.Var Type.TypeVarKey)
get_type_var_arena = reader (\ (ANFIR.ANFIR _ _ _ vars _ _ _) -> vars)

get_binding :: ANFIR.BindingKey -> IRReader captures ty poison_allowed (ANFIR.Binding captures ty poison_allowed)
get_binding k = reader (\ (ANFIR.ANFIR _ _ _ _ bindings _ _) -> Arena.get bindings k)
get_param :: ANFIR.ParamKey -> IRReader captures ty poison_allowed (ANFIR.Param ty)
get_param k = reader (\ (ANFIR.ANFIR _ _ _ _ _ params _) -> Arena.get params k)
get_adt :: Type.ADTKey -> IRReader captures ty poison_allowed (Type.ADT ty)
get_adt k = reader (\ (ANFIR.ANFIR _ adts _ _ _ _ _) -> Arena.get adts k)
get_type_synonym :: Type.TypeSynonymKey -> IRReader captures ty poison_allowed (Type.TypeSynonym ty)
get_type_synonym k = reader (\ (ANFIR.ANFIR _ _ type_synonyms _ _ _ _) -> Arena.get type_synonyms k)
get_type_var :: Type.TypeVarKey -> IRReader captures ty poison_allowed Type.Var
get_type_var k = reader (\ (ANFIR.ANFIR _ _ _ type_vars _ _ _) -> Arena.get type_vars k)

dump_main_module :: (DumpableCaptures captures, DumpableType ty) => ANFIR.ANFIR captures ty poison_allowed -> Text
dump_main_module ir@(ANFIR.ANFIR decls _ _ _ _ _ mod) = PP.render $ runReader (define_decl $ Arena.get decls mod) ir

define_decl :: (DumpableCaptures captures, DumpableType ty) => (ANFIR.Decl captures) -> IRReader captures ty poison_allowed PP.Token
define_decl (ANFIR.Decl'Module bindings adts type_synonyms) =
    ask >>= \ anfir ->
    mapM (fmap Type.PP.define_adt . get_adt) adts >>= \ adts ->
    mapM (fmap (Type.PP.define_type_synonym (\ ty -> runReader (refer_type ty) anfir)) . get_type_synonym) type_synonyms >>= \ type_synonyms ->
    define_binding_group_flat bindings >>= \ bindings ->
    pure (PP.flat_block $ adts <> type_synonyms <> bindings)
define_decl (ANFIR.Decl'Type _) = pure $ PP.List []

refer_param :: ANFIR.ParamKey -> IRReader captures ty poison_allowed PP.Token
refer_param key = get_param key >>= \ (ANFIR.Param id _) -> pure (PP.String (ID.stringify id))

refer_binding :: ANFIR.BindingKey -> IRReader captures ty poison_allowed PP.Token
refer_binding key = ANFIR.binding_id <$> get_binding key >>= \ id -> pure (PP.String (ANFIR.stringify_id id))

class DumpableCaptures captures where
    dump_captures :: captures -> IRReader captures ty poison_allowed [PP.Token]
instance DumpableCaptures () where
    dump_captures = const (pure [])
instance DumpableCaptures (Set ANFIR.BindingKey) where
    dump_captures = mapM refer_binding . toList

define_binding_group_flat :: (DumpableCaptures captures, DumpableType ty) => (ANFIR.BindingGroup captures) -> IRReader captures ty poison_allowed [PP.Token]
define_binding_group_flat (ANFIR.BindingGroup _ _ bindings) = mapM define_binding bindings
define_binding_group :: (DumpableCaptures captures, DumpableType ty) => (ANFIR.BindingGroup captures) -> IRReader captures ty poison_allowed PP.Token
define_binding_group (ANFIR.BindingGroup _ captures bindings) = mapM define_binding bindings >>= \ bindings -> dump_captures captures >>= \ captures -> pure (PP.braced_block $ if null captures then bindings else (PP.List ["capture ", PP.comma_separated PP.Inconsistent captures, ";"] : bindings))

define_binding :: (DumpableCaptures captures, DumpableType ty) => ANFIR.BindingKey -> IRReader captures ty poison_allowed PP.Token
define_binding key =
    get_binding key >>= \ (ANFIR.Binding _ e) ->
    refer_binding key >>= \ key ->
    expr e >>= \ e ->
    pure (PP.List [key, " = ", e, ";"])

class DumpableType t where
    refer_type :: t -> IRReader captures ty poison_allowed PP.Token

instance DumpableType (Maybe (Type.Type Void)) where -- TODO: remove this
    refer_type (Just ty) = refer_type ty
    refer_type Nothing = pure $ PP.String "<type error>"

instance DumpableType (Type.Type Void) where
    refer_type ty =
        get_adt_arena >>= \ adt_arena ->
        get_type_synonym_arena >>= \ type_synonym_arena ->
        get_type_var_arena >>= \ type_var_arena ->
        pure (Type.PP.refer_type absurd adt_arena type_synonym_arena type_var_arena ty)

type_var :: Type.TypeVarKey -> IRReader captures ty poison_allowed PP.Token
type_var k = get_type_var k >>= \ (Type.Var name) -> pure (PP.String name)

expr :: (DumpableCaptures captures, DumpableType ty) => ANFIR.Expr captures ty poison_allowed -> IRReader captures ty poison_allowed PP.Token
expr (ANFIR.Expr'Identifier _ _ bk) = refer_binding bk
expr (ANFIR.Expr'Int _ _ i) = pure $ PP.String $ show i
expr (ANFIR.Expr'Float _ _ (n :% d)) = pure $ PP.String $ "(" <> show n <> "/" <> show d <> ")"
expr (ANFIR.Expr'Bool _ _ b) = pure $ PP.String $ if b then "true" else "false"
expr (ANFIR.Expr'Char _ _ c) = pure $ PP.String $ show c
expr (ANFIR.Expr'String _ _ s) = pure $ PP.String $ show s
expr (ANFIR.Expr'Tuple _ _ a b) = refer_binding a >>= \ a -> refer_binding b >>= \ b -> pure (PP.parenthesized_comma_list PP.Inconsistent [a, b])
expr (ANFIR.Expr'Lambda _ _ param group body) = refer_param param >>= \ param -> define_binding_group group >>= \ group -> refer_binding body >>= \ body -> pure (PP.FirstOnLineIfMultiline $ PP.List ["\\ ", param, " -> ", group, "\n", body])
expr (ANFIR.Expr'Param _ _ pk) = refer_param pk
expr (ANFIR.Expr'Call _ _ callee arg) = refer_binding callee >>= \ callee -> refer_binding arg >>= \ arg -> pure (PP.List [callee, "(", arg, ")"])
expr (ANFIR.Expr'Switch _ _ e arms) = refer_binding e >>= \ e -> mapM arm arms >>= \ arms -> pure (PP.List ["switch ", e, " ", PP.braced_block arms])
    where
        arm (ANFIR.Switch'BoolLiteral b, group, expr) = define_binding_group group >>= \ group -> refer_binding expr >>= \ expr -> pure (PP.List [if b then "true" else "false", " -> ", group, "\n", expr, ";"])
        arm (ANFIR.Switch'Tuple, group, expr) = define_binding_group group >>= \ group -> refer_binding expr >>= \ expr -> pure (PP.List ["(,) -> ", group, "\n", expr, ";"])
        arm (ANFIR.Switch'Default, group, expr) = define_binding_group group >>= \ group -> refer_binding expr >>= \ expr -> pure (PP.List ["_ -> ", group, "\n", expr, ";"])
expr (ANFIR.Expr'Seq _ _ a b) = refer_binding a >>= \ a -> refer_binding b >>= \ b -> pure (PP.List ["seq ", a, ", ", b])
expr (ANFIR.Expr'TupleDestructure1 _ _ other) = refer_binding other >>= \ other ->  pure (PP.List [other, ".0"])
expr (ANFIR.Expr'TupleDestructure2 _ _ other) = refer_binding other >>= \ other ->  pure (PP.List [other, ".1"])
expr (ANFIR.Expr'Forall _ _ vars group e) = mapM type_var vars >>= \ vars -> define_binding_group group >>= \ group -> refer_binding e >>= \ e -> pure (PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent $ toList vars, " ", group, "\n", e])
expr (ANFIR.Expr'TypeApply _ _ e arg) = refer_binding e >>= \ e -> refer_type arg >>= \ arg -> pure (PP.List [e, "#(", arg, ")"])
expr (ANFIR.Expr'MakeADT _ _ variant_index@(Type.ADTVariantIndex adt_key _) args) =
    Type.PP.refer_adt <$> get_adt adt_key >>= \ adt_referred ->
    Type.get_adt_variant <$> get_adt_arena <*> pure variant_index >>= \ variant ->
    mapM refer_binding args >>= \ args ->
    let variant_name = Type.variant_name variant
    in pure $ PP.List ["adt ", adt_referred, " ", PP.String variant_name, PP.bracketed_comma_list PP.Inconsistent args]
expr (ANFIR.Expr'Poison _ _ _) = pure $ PP.String "poison"
