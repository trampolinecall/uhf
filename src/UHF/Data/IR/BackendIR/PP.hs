{-# LANGUAGE FlexibleInstances #-}

module UHF.Data.IR.BackendIR.PP (dump_cu) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.PP as PP

import qualified UHF.Data.IR.BackendIR as BackendIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.PP as Type.PP
import qualified UHF.Data.IR.ID as ID

import UHF.IO.Located (Located (Located, unlocate))

import qualified Data.Set as Set

-- TODO: dump types too

type IRReader ty poison_allowed = Reader (BackendIR.BackendIR ty poison_allowed)

get_adt_arena :: IRReader ty poison_allowed (Arena.Arena (Type.ADT ty) Type.ADTKey)
get_adt_arena = reader (\ (BackendIR.BackendIR adts _ _ _ _ _) -> adts)
get_type_synonym_arena :: IRReader ty poison_allowed (Arena.Arena (Type.TypeSynonym ty) Type.TypeSynonymKey)
get_type_synonym_arena = reader (\ (BackendIR.BackendIR _ syns _ _ _ _) -> syns)
get_type_var_arena :: IRReader ty poison_allowed (Arena.Arena Type.Var Type.TypeVarKey)
get_type_var_arena = reader (\ (BackendIR.BackendIR _ _ vars _ _ _) -> vars)

get_binding :: BackendIR.BindingKey -> IRReader ty poison_allowed (BackendIR.Binding ty poison_allowed)
get_binding k = reader (\ (BackendIR.BackendIR _ _ _ bindings _ _) -> Arena.get bindings k)
get_param :: BackendIR.ParamKey -> IRReader ty poison_allowed (BackendIR.Param ty)
get_param k = reader (\ (BackendIR.BackendIR _ _ _ _ params _) -> Arena.get params k)
get_adt :: Type.ADTKey -> IRReader ty poison_allowed (Type.ADT ty)
get_adt k = reader (\ (BackendIR.BackendIR adts _ _ _ _ _) -> Arena.get adts k)
get_type_synonym :: Type.TypeSynonymKey -> IRReader ty poison_allowed (Type.TypeSynonym ty)
get_type_synonym k = reader (\ (BackendIR.BackendIR _ type_synonyms _ _ _ _) -> Arena.get type_synonyms k)
get_type_var :: Type.TypeVarKey -> IRReader ty poison_allowed Type.Var
get_type_var k = reader (\ (BackendIR.BackendIR _ _ type_vars _ _ _) -> Arena.get type_vars k)

dump_cu :: (DumpableType ty) => BackendIR.BackendIR ty poison_allowed -> Text
dump_cu ir@(BackendIR.BackendIR _ _ _ _ _ cu) = PP.render $ runReader (define_cu cu) ir

define_cu :: (DumpableType ty) => BackendIR.CU -> IRReader ty poison_allowed PP.Token
define_cu (BackendIR.CU bindings adts type_synonyms) =
    ask >>= \ anfir ->
    mapM (fmap Type.PP.define_adt . get_adt) adts >>= \ adts ->
    mapM (fmap (Type.PP.define_type_synonym (\ ty -> runReader (refer_type ty) anfir)) . get_type_synonym) type_synonyms >>= \ type_synonyms ->
    define_binding_group_flat bindings >>= \ bindings ->
    pure (PP.flat_block $ adts <> type_synonyms <> bindings)

refer_param :: BackendIR.ParamKey -> IRReader ty poison_allowed PP.Token
refer_param key = get_param key >>= \ (BackendIR.Param id _) -> pure (PP.String (ID.stringify id))

refer_binding :: BackendIR.BindingKey -> IRReader ty poison_allowed PP.Token
refer_binding key = BackendIR.binding_id <$> get_binding key >>= \ id -> pure (PP.String (BackendIR.stringify_id id))

dump_captures :: Set.Set BackendIR.BindingKey -> IRReader ty poison_allowed [PP.Token]
dump_captures = mapM refer_binding . toList

define_binding_group_flat :: (DumpableType ty) => BackendIR.BindingGroup -> IRReader ty poison_allowed [PP.Token]
define_binding_group_flat (BackendIR.BindingGroup _ chunks) = mapM define_chunk chunks
define_binding_group :: (DumpableType ty) => BackendIR.BindingGroup -> IRReader ty poison_allowed PP.Token
define_binding_group (BackendIR.BindingGroup captures chunks) = mapM define_chunk chunks >>= \ chunks -> dump_captures captures >>= \ captures -> pure (PP.braced_block $ if null captures then chunks else PP.List ["capture ", PP.comma_separated PP.Inconsistent captures, ";"] : chunks)

define_chunk :: (DumpableType ty) => BackendIR.BindingChunk -> IRReader ty poison_allowed PP.Token
define_chunk (BackendIR.SingleBinding bk) = define_binding bk
define_chunk (BackendIR.MutuallyRecursiveBindings bindings) = mapM define_binding bindings >>= \ bindings -> pure (PP.List ["mutually recursive ", PP.braced_block bindings])

define_binding :: (DumpableType ty) => BackendIR.BindingKey -> IRReader ty poison_allowed PP.Token
define_binding key =
    get_binding key >>= \ (BackendIR.Binding e) ->
    refer_binding key >>= \ key ->
    expr e >>= \ e ->
    pure (PP.List [key, " = ", e, ";"])

class DumpableType t where
    refer_type :: t -> IRReader ty poison_allowed PP.Token

instance DumpableType (Maybe (Type.Type Void)) where -- TODO: remove this
    refer_type (Just ty) = refer_type ty
    refer_type Nothing = pure $ PP.String "<type error>"

instance DumpableType (Type.Type Void) where
    refer_type ty =
        get_adt_arena >>= \ adt_arena ->
        get_type_synonym_arena >>= \ type_synonym_arena ->
        get_type_var_arena >>= \ type_var_arena ->
        pure (Type.PP.refer_type absurd adt_arena type_synonym_arena type_var_arena ty)

type_var :: Type.TypeVarKey -> IRReader ty poison_allowed PP.Token
type_var k = get_type_var k >>= \ (Type.Var (Located _ name)) -> pure (PP.String name)

expr :: (DumpableType ty) => BackendIR.Expr ty poison_allowed -> IRReader ty poison_allowed PP.Token
expr (BackendIR.Expr'Refer _ _ bk) = refer_binding bk
expr (BackendIR.Expr'Int _ _ i) = pure $ PP.String $ show i
expr (BackendIR.Expr'Float _ _ (n :% d)) = pure $ PP.String $ "(" <> show n <> "/" <> show d <> ")"
expr (BackendIR.Expr'Bool _ _ b) = pure $ PP.String $ if b then "true" else "false"
expr (BackendIR.Expr'Char _ _ c) = pure $ PP.String $ show c
expr (BackendIR.Expr'String _ _ s) = pure $ PP.String $ show s
expr (BackendIR.Expr'Tuple _ _ a b) = refer_binding a >>= \ a -> refer_binding b >>= \ b -> pure (PP.parenthesized_comma_list PP.Inconsistent [a, b])
expr (BackendIR.Expr'Lambda _ _ param group body) = refer_param param >>= \ param -> define_binding_group group >>= \ group -> refer_binding body >>= \ body -> pure (PP.FirstOnLineIfMultiline $ PP.List ["\\ ", param, " ->", PP.indented_block [group, body]])
expr (BackendIR.Expr'Param _ _ pk) = refer_param pk
expr (BackendIR.Expr'Call _ _ callee arg) = refer_binding callee >>= \ callee -> refer_binding arg >>= \ arg -> pure (PP.List [callee, "(", arg, ")"])
expr (BackendIR.Expr'Switch _ _ e arms) = refer_binding e >>= \ e -> mapM arm arms >>= \ arms -> pure (PP.List ["switch ", e, " ", PP.braced_block arms])
    where
        arm (BackendIR.Switch'BoolLiteral b, group, expr) = define_binding_group group >>= \ group -> refer_binding expr >>= \ expr -> pure (PP.List [if b then "true" else "false", " -> ", PP.indented_block [group, expr], ";"])
        arm (BackendIR.Switch'Tuple, group, expr) = define_binding_group group >>= \ group -> refer_binding expr >>= \ expr -> pure (PP.List ["(,) -> ", PP.indented_block [group, expr], ";"])
        arm (BackendIR.Switch'Default, group, expr) = define_binding_group group >>= \ group -> refer_binding expr >>= \ expr -> pure (PP.List ["_ -> ", PP.indented_block [group, expr], ";"])
expr (BackendIR.Expr'TupleDestructure1 _ _ other) = refer_binding other >>= \ other ->  pure (PP.List [other, ".0"])
expr (BackendIR.Expr'TupleDestructure2 _ _ other) = refer_binding other >>= \ other ->  pure (PP.List [other, ".1"])
expr (BackendIR.Expr'Forall _ _ vars group e) = mapM type_var vars >>= \ vars -> define_binding_group group >>= \ group -> refer_binding e >>= \ e -> pure (PP.FirstOnLineIfMultiline $ PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent $ toList vars, " ", PP.indented_block [group, e]])
expr (BackendIR.Expr'TypeApply _ _ e arg) = refer_binding e >>= \ e -> refer_type arg >>= \ arg -> pure (PP.List [e, "#(", arg, ")"])
expr (BackendIR.Expr'MakeADT _ _ variant_index@(Type.ADTVariantIndex adt_key _) args) =
    Type.PP.refer_adt <$> get_adt adt_key >>= \ adt_referred ->
    Type.get_adt_variant <$> get_adt_arena <*> pure variant_index >>= \ variant ->
    mapM refer_binding args >>= \ args ->
    let variant_name = unlocate $ Type.variant_name variant
    in pure $ PP.List ["adt ", adt_referred, " ", PP.String variant_name, PP.bracketed_comma_list PP.Inconsistent args]
expr (BackendIR.Expr'Poison _ _ _) = pure $ PP.String "poison"
