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

type IRReader = Reader ANFIR.ANFIR

get_adt_arena :: IRReader (Arena.Arena (Type.ADT (Maybe (Type.Type Void))) Type.ADTKey)
get_adt_arena = reader (\ (ANFIR.ANFIR _ adts _ _ _ _ _) -> adts)
get_type_synonym_arena :: IRReader (Arena.Arena (Type.TypeSynonym (Maybe (Type.Type Void))) Type.TypeSynonymKey)
get_type_synonym_arena = reader (\ (ANFIR.ANFIR _ _ syns _ _ _ _) -> syns)
get_type_var_arena :: IRReader (Arena.Arena Type.Var Type.TypeVarKey)
get_type_var_arena = reader (\ (ANFIR.ANFIR _ _ _ vars _ _ _) -> vars)

get_binding :: ANFIR.BindingKey -> IRReader ANFIR.Binding
get_binding k = reader (\ (ANFIR.ANFIR _ _ _ _ bindings _ _) -> Arena.get bindings k)
get_param :: ANFIR.ParamKey -> IRReader ANFIR.Param
get_param k = reader (\ (ANFIR.ANFIR _ _ _ _ _ params _) -> Arena.get params k)
get_adt :: Type.ADTKey -> IRReader (Type.ADT (Maybe (Type.Type Void)))
get_adt k = reader (\ (ANFIR.ANFIR _ adts _ _ _ _ _) -> Arena.get adts k)
get_type_synonym :: Type.TypeSynonymKey -> IRReader (Type.TypeSynonym (Maybe (Type.Type Void)))
get_type_synonym k = reader (\ (ANFIR.ANFIR _ _ type_synonyms _ _ _ _) -> Arena.get type_synonyms k)
get_type_var :: Type.TypeVarKey -> IRReader Type.Var
get_type_var k = reader (\ (ANFIR.ANFIR _ _ _ type_vars _ _ _) -> Arena.get type_vars k)

dump_main_module :: ANFIR.ANFIR -> Text
dump_main_module ir@(ANFIR.ANFIR decls _ _ _ _ _ mod) = PP.render $ runReader (define_decl $ Arena.get decls mod) ir

define_decl :: ANFIR.Decl -> IRReader PP.Token
define_decl (ANFIR.Decl'Module bindings adts type_synonyms) =
    ask >>= \ anfir ->
    mapM (fmap Type.PP.define_adt . get_adt) adts >>= \ adts ->
    mapM (fmap (Type.PP.define_type_synonym (\ ty -> runReader (refer_type ty) anfir)) . get_type_synonym) type_synonyms >>= \ type_synonyms ->
    define_binding_group_flat bindings >>= \ bindings ->
    pure (PP.flat_block $ adts <> type_synonyms <> bindings)
define_decl (ANFIR.Decl'Type _) = pure $ PP.List []

refer_param :: ANFIR.ParamKey -> IRReader PP.Token
refer_param key = get_param key >>= \ (ANFIR.Param id _) -> pure (PP.String (ID.stringify id))

refer_binding :: ANFIR.BindingKey -> IRReader PP.Token
refer_binding key = ANFIR.binding_id <$> get_binding key >>= \ id -> pure (PP.String (ANFIR.stringify_id id))

define_binding_group_flat :: ANFIR.BindingGroup -> IRReader [PP.Token]
define_binding_group_flat (ANFIR.BindingGroup bindings) = mapM define_binding bindings
define_binding_group :: ANFIR.BindingGroup -> IRReader PP.Token
define_binding_group (ANFIR.BindingGroup bindings) = mapM define_binding bindings >>= \ bindings -> pure (PP.braced_block bindings)

define_binding :: ANFIR.BindingKey -> IRReader PP.Token
define_binding key =
    get_binding key >>= \ (ANFIR.Binding e) ->
    refer_binding key >>= \ key ->
    expr e >>= \ e ->
    pure (PP.List [key, " = ", e, ";"])

-- TODO: remove this
refer_type :: Maybe (Type.Type Void) -> IRReader PP.Token
refer_type (Just ty) =
    get_adt_arena >>= \ adt_arena ->
    get_type_synonym_arena >>= \ type_synonym_arena ->
    get_type_var_arena >>= \ type_var_arena ->
    pure (Type.PP.refer_type absurd adt_arena type_synonym_arena type_var_arena ty)
refer_type Nothing = pure $ PP.String "<type error>"

type_var :: Type.TypeVarKey -> IRReader PP.Token
type_var k = get_type_var k >>= \ (Type.Var name) -> pure (PP.String name)

expr :: ANFIR.Expr -> IRReader PP.Token
expr (ANFIR.Expr'Refer _ _ bk) = refer_binding bk
expr (ANFIR.Expr'Int _ _ i) = pure $ PP.String $ show i
expr (ANFIR.Expr'Float _ _ (n :% d)) = pure $ PP.String $ "(" <> show n <> "/" <> show d <> ")"
expr (ANFIR.Expr'Bool _ _ b) = pure $ PP.String $ if b then "true" else "false"
expr (ANFIR.Expr'Char _ _ c) = pure $ PP.String $ show c
expr (ANFIR.Expr'String _ _ s) = pure $ PP.String $ show s
expr (ANFIR.Expr'Tuple _ _ a b) = refer_binding a >>= \ a -> refer_binding b >>= \ b -> pure (PP.parenthesized_comma_list PP.Inconsistent [a, b])
expr (ANFIR.Expr'Lambda _ _ param group body) = refer_param param >>= \ param -> define_binding_group group >>= \ group -> refer_binding body >>= \ body -> pure (PP.FirstOnLineIfMultiline $ PP.List ["\\ ", param, " ->", PP.indented_block [group, body]])
expr (ANFIR.Expr'Param _ _ pk) = refer_param pk
expr (ANFIR.Expr'Call _ _ callee arg) = refer_binding callee >>= \ callee -> refer_binding arg >>= \ arg -> pure (PP.List [callee, "(", arg, ")"])
expr (ANFIR.Expr'Switch _ _ e arms) = refer_binding e >>= \ e -> mapM arm arms >>= \ arms -> pure (PP.List ["switch ", e, " ", PP.braced_block arms])
    where
        arm (ANFIR.Switch'BoolLiteral b, group, expr) = define_binding_group group >>= \ group -> refer_binding expr >>= \ expr -> pure (PP.List [if b then "true" else "false", " -> ", PP.indented_block [group, expr], ";"])
        arm (ANFIR.Switch'Tuple, group, expr) = define_binding_group group >>= \ group -> refer_binding expr >>= \ expr -> pure (PP.List ["(,) -> ", PP.indented_block [group, expr], ";"])
        arm (ANFIR.Switch'Default, group, expr) = define_binding_group group >>= \ group -> refer_binding expr >>= \ expr -> pure (PP.List ["_ -> ", PP.indented_block [group, expr], ";"])
expr (ANFIR.Expr'Seq _ _ a b) = refer_binding a >>= \ a -> refer_binding b >>= \ b -> pure (PP.List ["seq ", a, ", ", b])
expr (ANFIR.Expr'TupleDestructure1 _ _ other) = refer_binding other >>= \ other ->  pure (PP.List [other, ".0"])
expr (ANFIR.Expr'TupleDestructure2 _ _ other) = refer_binding other >>= \ other ->  pure (PP.List [other, ".1"])
expr (ANFIR.Expr'Forall _ _ vars group e) = mapM type_var vars >>= \ vars -> define_binding_group group >>= \ group -> refer_binding e >>= \ e -> pure (PP.FirstOnLineIfMultiline $ PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent $ toList vars, " ", PP.indented_block [group, e]])
expr (ANFIR.Expr'TypeApply _ _ e arg) = refer_binding e >>= \ e -> refer_type arg >>= \ arg -> pure (PP.List [e, "#(", arg, ")"])
expr (ANFIR.Expr'MakeADT _ _ variant_index@(Type.ADTVariantIndex adt_key _) args) =
    Type.PP.refer_adt <$> get_adt adt_key >>= \ adt_referred ->
    Type.get_adt_variant <$> get_adt_arena <*> pure variant_index >>= \ variant ->
    mapM refer_binding args >>= \ args ->
    let variant_name = Type.variant_name variant
    in pure $ PP.List ["adt ", adt_referred, " ", PP.String variant_name, PP.bracketed_comma_list PP.Inconsistent args]
expr (ANFIR.Expr'Poison _ _) = pure $ PP.String "poison"
