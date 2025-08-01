{-# LANGUAGE FlexibleInstances #-}

module UHF.Data.ANFIR.PP (dump_cu) where

import UHF.Prelude

import UHF.Source.Located (Located (Located, unlocate))
import qualified UHF.Data.ANFIR as ANFIR
import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.IR.Intrinsics as Intrinsics
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.IR.Type.PP as Type.PP
import qualified UHF.PP as PP
import qualified UHF.Util.Arena as Arena

-- TODO: dump types too

type IRReader = Reader ANFIR.ANFIR

get_adt_arena :: IRReader (Arena.Arena (Type.ADT (Maybe Type.Type)) Type.ADTKey)
get_adt_arena = reader (\ (ANFIR.ANFIR adts _ _ _ _ _) -> adts)
get_type_synonym_arena :: IRReader (Arena.Arena (Type.TypeSynonym (Maybe Type.Type)) Type.TypeSynonymKey)
get_type_synonym_arena = reader (\ (ANFIR.ANFIR _ syns _ _ _ _) -> syns)
get_quant_var_arena :: IRReader (Arena.Arena Type.QuantVar Type.QuantVarKey)
get_quant_var_arena = reader (\ (ANFIR.ANFIR _ _ vars _ _ _) -> vars)

get_binding :: ANFIR.BindingKey -> IRReader ANFIR.Binding
get_binding k = reader (\ (ANFIR.ANFIR _ _ _ bindings _ _) -> Arena.get bindings k)
get_param :: ANFIR.ParamKey -> IRReader ANFIR.Param
get_param k = reader (\ (ANFIR.ANFIR _ _ _ _ params _) -> Arena.get params k)
get_adt :: Type.ADTKey -> IRReader (Type.ADT (Maybe Type.Type))
get_adt k = reader (\ (ANFIR.ANFIR adts _ _ _ _ _) -> Arena.get adts k)
get_type_synonym :: Type.TypeSynonymKey -> IRReader (Type.TypeSynonym (Maybe Type.Type))
get_type_synonym k = reader (\ (ANFIR.ANFIR _ type_synonyms _ _ _ _) -> Arena.get type_synonyms k)
get_quant_var :: Type.QuantVarKey -> IRReader Type.QuantVar
get_quant_var k = reader (\ (ANFIR.ANFIR _ _ quant_vars _ _ _) -> Arena.get quant_vars k)

dump_cu :: ANFIR.ANFIR -> Text
dump_cu ir@(ANFIR.ANFIR _ _ _ _ _ cu) = PP.render $ runReader (define_cu cu) ir

define_cu :: ANFIR.CU -> IRReader PP.Token
define_cu (ANFIR.CU _ bindings adts type_synonyms) =
    ask >>= \ anfir ->
    get_quant_var_arena >>= \ quant_var_arena ->
    mapM (fmap (Type.PP.define_adt quant_var_arena (\ ty -> runReader (refer_type ty) anfir)) . get_adt) adts >>= \ adts ->
    mapM (fmap (Type.PP.define_type_synonym (\ ty -> runReader (refer_type ty) anfir)) . get_type_synonym) type_synonyms >>= \ type_synonyms ->
    define_binding_group_flat bindings >>= \ bindings ->
    pure (PP.flat_block $ adts <> type_synonyms <> bindings)

refer_param :: ANFIR.ParamKey -> IRReader PP.Token
refer_param key = get_param key >>= \ (ANFIR.Param id _) -> pure (PP.String (ID.stringify id))

refer_binding :: ANFIR.BindingKey -> IRReader PP.Token
refer_binding key = ANFIR.binding_id <$> get_binding key >>= \ id -> pure (PP.String (ANFIR.stringify_id id))

define_binding_group_flat :: ANFIR.BindingGroup -> IRReader [PP.Token]
define_binding_group_flat (ANFIR.BindingGroup _ bindings) = mapM define_binding bindings
define_binding_group :: ANFIR.BindingGroup -> IRReader PP.Token
define_binding_group (ANFIR.BindingGroup _ bindings) = mapM define_binding bindings >>= \ bindings -> pure (PP.braced_block bindings)

define_binding :: ANFIR.BindingKey -> IRReader PP.Token
define_binding key =
    get_binding key >>= \ (ANFIR.Binding e) ->
    refer_binding key >>= \ key ->
    expr e >>= \ e ->
    pure (PP.List [key, " = ", e, ";"])

-- TODO: remove this
refer_type :: Maybe Type.Type -> IRReader PP.Token
refer_type (Just ty) =
    get_adt_arena >>= \ adt_arena ->
    get_type_synonym_arena >>= \ type_synonym_arena ->
    get_quant_var_arena >>= \ quant_var_arena ->
    pure (Type.PP.refer_type adt_arena type_synonym_arena quant_var_arena ty)
refer_type Nothing = pure $ PP.String "<type error>"

quant_var :: Type.QuantVarKey -> IRReader PP.Token
quant_var k = get_quant_var k >>= \ (Type.QuantVar (Located _ name)) -> pure (PP.String name)

expr :: ANFIR.Expr -> IRReader PP.Token
expr (ANFIR.Expr'Refer _ _ bk) = refer_binding bk
expr (ANFIR.Expr'Intrinsic _ _ i) = pure $ PP.String $ Intrinsics.intrinsic_name i
expr (ANFIR.Expr'Int _ _ i) = pure $ PP.String $ show i
expr (ANFIR.Expr'Float _ _ (n :% d)) = pure $ PP.String $ "(" <> show n <> "/" <> show d <> ")"
expr (ANFIR.Expr'Bool _ _ b) = pure $ PP.String $ if b then "true" else "false"
expr (ANFIR.Expr'Char _ _ c) = pure $ PP.String $ show c
expr (ANFIR.Expr'String _ _ s) = pure $ PP.String $ show s
expr (ANFIR.Expr'Tuple _ _ a b) = refer_binding a >>= \ a -> refer_binding b >>= \ b -> pure (PP.parenthesized_comma_list PP.Inconsistent [a, b])
expr (ANFIR.Expr'Lambda _ _ param captures group body) = refer_param param >>= \ param -> mapM refer_binding (toList captures) >>= \ captures -> define_binding_group group >>= \ group -> refer_binding body >>= \ body -> pure (PP.FirstOnLineIfMultiline $ PP.List ["\\ ", param, "[", PP.comma_separated PP.Inconsistent captures, "] ->", PP.indented_block [group, body]])
expr (ANFIR.Expr'Param _ _ pk) = refer_param pk
expr (ANFIR.Expr'Call _ _ callee arg) = refer_binding callee >>= \ callee -> refer_binding arg >>= \ arg -> pure (PP.List [callee, "(", arg, ")"])
expr (ANFIR.Expr'Match _ _ t) = tree t >>= \ t -> pure (PP.List ["match ", t])
    where
        tree (ANFIR.MatchTree arms) = mapM arm arms >>= \ arms -> pure (PP.braced_block arms)

        arm (clauses, result) =
            mapM clause clauses >>= \ clauses ->
            (case result of
                Right (group, expr) ->
                    define_binding_group group >>= \ group -> refer_binding expr >>= \ expr ->
                    pure (PP.indented_block [group, expr])
                Left subtree -> tree subtree) >>= \ result ->
            pure (PP.List [PP.bracketed_comma_list PP.Inconsistent clauses, " -> ", result, ";"])

        clause (ANFIR.MatchClause'Match b m) = refer_binding b >>= \ b -> matcher m >>= \ matcher -> pure (PP.List [b, " -> ", matcher])
        clause (ANFIR.MatchClause'Binding b) = define_binding b

        matcher (ANFIR.Match'BoolLiteral b) = pure $ if b then "true" else "false"
        matcher ANFIR.Match'Tuple = pure "(,)"
        matcher (ANFIR.Match'AnonADTVariant m_variant) =
            maybe
                (pure "<name resolution error>")
                (\ variant_index@(Type.ADT.VariantIndex _ adt_key _) ->
                    Type.PP.refer_adt <$> get_adt adt_key >>= \ adt_refer ->
                    Type.ADT.get_variant <$> get_adt_arena <*> pure variant_index >>= \ variant ->
                    let variant_name = Type.ADT.variant_name variant
                    in pure $ PP.List [adt_refer, " ", PP.String $ unlocate variant_name]
                )
                m_variant

expr (ANFIR.Expr'TupleDestructure1 _ _ other) = refer_binding other >>= \ other ->  pure (PP.List [other, ".tuple_l"])
expr (ANFIR.Expr'TupleDestructure2 _ _ other) = refer_binding other >>= \ other ->  pure (PP.List [other, ".tuple_r"])
expr (ANFIR.Expr'ADTDestructure _ _ base m_field_idx) =
    refer_binding base >>= \ base ->
    maybe
        (pure ("<error>", "<error>"))
        (\ (Type.ADT.FieldIndex _ variant_idx@(Type.ADT.VariantIndex _ adt_key _) field_idx) ->
            Type.PP.refer_adt <$> get_adt adt_key >>= \ adt_referred ->
            Type.ADT.get_variant <$> get_adt_arena <*> pure variant_idx >>= \ variant ->
            let variant_name = Type.ADT.variant_name variant
            in pure (PP.List [adt_referred, " ", PP.String $ unlocate variant_name], PP.String $ show field_idx)
        )
        m_field_idx >>= \ (variant_referred, field) ->
    pure (PP.List ["(", base, " as ", variant_referred, ").", field])
expr (ANFIR.Expr'Forall _ _ var group e) = quant_var var >>= \ var -> define_binding_group group >>= \ group -> refer_binding e >>= \ e -> pure (PP.FirstOnLineIfMultiline $ PP.List ["#(", var, ") ", PP.indented_block [group, e]])
expr (ANFIR.Expr'TypeApply _ _ e arg) = refer_binding e >>= \ e -> refer_type arg >>= \ arg -> pure (PP.List [e, "#(", arg, ")"])
expr (ANFIR.Expr'MakeADT _ _ variant_index@(Type.ADT.VariantIndex _ adt_key _) tyargs args) =
    Type.PP.refer_adt <$> get_adt adt_key >>= \ adt_referred ->
    Type.ADT.get_variant <$> get_adt_arena <*> pure variant_index >>= \ variant ->
    mapM refer_binding args >>= \ args ->
    mapM refer_type tyargs >>= \ tyargs ->
    let variant_name = Type.ADT.variant_name variant
    in pure $ PP.List ["adt ", adt_referred, " ", PP.String $ unlocate variant_name, "#", PP.parenthesized_comma_list PP.Inconsistent tyargs, PP.bracketed_comma_list PP.Inconsistent args]
expr (ANFIR.Expr'Poison _ _) = pure $ PP.String "poison"
