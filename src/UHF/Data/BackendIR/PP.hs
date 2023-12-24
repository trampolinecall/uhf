{-# LANGUAGE FlexibleInstances #-}

module UHF.Data.BackendIR.PP (dump_cu) where

import UHF.Prelude

import qualified Data.Set as Set

import UHF.Source.Located (Located (Located, unlocate))
import qualified UHF.Data.BackendIR as BackendIR
import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.IR.Type.PP as Type.PP
import qualified UHF.PP as PP
import qualified UHF.Util.Arena as Arena

-- TODO: dump types too

type IRReader ty poison_allowed = Reader (BackendIR.BackendIR ty poison_allowed)

get_adt_arena :: IRReader ty poison_allowed (Arena.Arena (Type.ADT ty) Type.ADTKey)
get_adt_arena = reader (\ (BackendIR.BackendIR adts _ _ _ _ _ _ _) -> adts)
get_type_synonym_arena :: IRReader ty poison_allowed (Arena.Arena (Type.TypeSynonym ty) Type.TypeSynonymKey)
get_type_synonym_arena = reader (\ (BackendIR.BackendIR _ syns _ _ _ _ _ _) -> syns)
get_type_var_arena :: IRReader ty poison_allowed (Arena.Arena Type.QuantVar Type.QuantVarKey)
get_type_var_arena = reader (\ (BackendIR.BackendIR _ _ _ _ vars _ _ _) -> vars)

get_binding :: BackendIR.BindingKey -> IRReader ty poison_allowed (BackendIR.Binding ty poison_allowed)
get_binding k = reader (\ (BackendIR.BackendIR _ _ _ _ _ bindings _ _) -> Arena.get bindings k)
get_param :: BackendIR.ParamKey -> IRReader ty poison_allowed (BackendIR.Param ty)
get_param k = reader (\ (BackendIR.BackendIR _ _ _ _ _ _ params _) -> Arena.get params k)
get_adt :: Type.ADTKey -> IRReader ty poison_allowed (Type.ADT ty)
get_adt k = reader (\ (BackendIR.BackendIR adts _ _ _ _ _ _ _) -> Arena.get adts k)
get_type_synonym :: Type.TypeSynonymKey -> IRReader ty poison_allowed (Type.TypeSynonym ty)
get_type_synonym k = reader (\ (BackendIR.BackendIR _ type_synonyms _ _ _ _ _ _) -> Arena.get type_synonyms k)
get_type_var :: Type.QuantVarKey -> IRReader ty poison_allowed Type.QuantVar
get_type_var k = reader (\ (BackendIR.BackendIR _ _ _ _ type_vars _ _ _) -> Arena.get type_vars k)
get_class :: Type.ClassKey -> IRReader ty poison_allowed Type.Class
get_class k = reader (\ (BackendIR.BackendIR _ _ classes _ _ _ _ _ ) -> Arena.get classes k)
get_instance :: Type.InstanceKey -> IRReader ty poison_allowed (Type.Instance (Maybe Type.ClassKey) ty)
get_instance k = reader (\ (BackendIR.BackendIR _ _ _ instances _ _ _ _) -> Arena.get instances k)

dump_cu :: (DumpableType ty) => BackendIR.BackendIR ty poison_allowed -> Text
dump_cu ir@(BackendIR.BackendIR _ _ _ _ _ _ _ cu) = PP.render $ runReader (define_cu cu) ir

define_cu :: (DumpableType ty) => BackendIR.CU -> IRReader ty poison_allowed PP.Token
define_cu (BackendIR.CU bindings adts type_synonyms classes instances) =
    ask >>= \ ir ->
    mapM (fmap Type.PP.define_adt . get_adt) adts >>= \ adts ->
    mapM (fmap (Type.PP.define_type_synonym (\ ty -> runReader (refer_type ty) ir)) . get_type_synonym) type_synonyms >>= \ type_synonyms ->
    mapM (fmap (Type.PP.define_class) . get_class) classes >>= \ classes ->
    mapM (fmap (Type.PP.define_instance (maybe "<class resolution error>" (\ cl -> runReader (Type.PP.refer_class <$> get_class cl) ir)) (\ ty -> runReader (refer_type ty) ir)) . get_instance) instances >>= \ instances ->
    define_binding_group_flat bindings >>= \ bindings ->
    pure (PP.flat_block $ adts <> type_synonyms <> classes <> instances <> bindings)

refer_param :: BackendIR.ParamKey -> IRReader ty poison_allowed PP.Token
refer_param key = get_param key >>= \ (BackendIR.Param id _) -> pure (PP.String (ID.stringify id))

refer_binding :: BackendIR.BindingKey -> IRReader ty poison_allowed PP.Token
refer_binding key = BackendIR.binding_id <$> get_binding key >>= \ id -> pure (PP.String (BackendIR.stringify_id id))

dump_captures :: Set.Set BackendIR.BindingKey -> IRReader ty poison_allowed [PP.Token]
dump_captures = mapM refer_binding . toList

define_binding_group_flat :: (DumpableType ty) => BackendIR.BindingGroup -> IRReader ty poison_allowed [PP.Token]
define_binding_group_flat (BackendIR.BindingGroup chunks) = mapM define_chunk chunks
define_binding_group :: (DumpableType ty) => BackendIR.BindingGroup -> IRReader ty poison_allowed PP.Token
define_binding_group (BackendIR.BindingGroup chunks) = mapM define_chunk chunks >>= \ chunks -> pure (PP.braced_block chunks)

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

instance DumpableType (Maybe Type.Type) where -- TODO: remove this
    refer_type (Just ty) = refer_type ty
    refer_type Nothing = pure $ PP.String "<type error>"

instance DumpableType Type.Type where
    refer_type ty =
        ask >>= \ (BackendIR.BackendIR adt_arena type_synonym_arena class_arena _ quant_var_arena _ _ _) ->
        pure (Type.PP.refer_type adt_arena type_synonym_arena class_arena quant_var_arena ty)

type_var :: Type.QuantVarKey -> IRReader ty poison_allowed PP.Token
type_var k = get_type_var k >>= \ (Type.QuantVar (Located _ name)) -> pure (PP.String name)

expr :: (DumpableType ty) => BackendIR.Expr ty poison_allowed -> IRReader ty poison_allowed PP.Token
expr (BackendIR.Expr'Refer _ _ bk) = refer_binding bk
expr (BackendIR.Expr'Int _ _ i) = pure $ PP.String $ show i
expr (BackendIR.Expr'Float _ _ (n :% d)) = pure $ PP.String $ "(" <> show n <> "/" <> show d <> ")"
expr (BackendIR.Expr'Bool _ _ b) = pure $ PP.String $ if b then "true" else "false"
expr (BackendIR.Expr'Char _ _ c) = pure $ PP.String $ show c
expr (BackendIR.Expr'String _ _ s) = pure $ PP.String $ show s
expr (BackendIR.Expr'Tuple _ _ a b) = refer_binding a >>= \ a -> refer_binding b >>= \ b -> pure (PP.parenthesized_comma_list PP.Inconsistent [a, b])
expr (BackendIR.Expr'Lambda _ _ param captures group body) = refer_param param >>= \ param -> define_binding_group group >>= \ group -> refer_binding body >>= \ body -> pure (PP.FirstOnLineIfMultiline $ PP.List ["\\ ", param, " ->", PP.indented_block [group, body]]) -- TODO: show captures
expr (BackendIR.Expr'Param _ _ pk) = refer_param pk
expr (BackendIR.Expr'Call _ _ callee arg) = refer_binding callee >>= \ callee -> refer_binding arg >>= \ arg -> pure (PP.List [callee, "(", arg, ")"])
expr (BackendIR.Expr'Let _ _ group result) = do
    group <- define_binding_group group
    result <- refer_binding result
    pure (PP.FirstOnLineIfMultiline $ PP.List ["let ", group, result])
expr (BackendIR.Expr'Match _ _ t) = tree t >>= \ t -> pure (PP.List ["match ", t])
    where
        tree (BackendIR.MatchTree arms) = mapM arm arms >>= \ arms -> pure (PP.braced_block arms)

        arm (clauses, result) =
            mapM clause clauses >>= \ clauses ->
            (case result of
                Right (group, expr) ->
                    define_binding_group group >>= \ group -> refer_binding expr >>= \ expr ->
                    pure (PP.indented_block [group, expr])
                Left subtree -> tree subtree) >>= \ result ->
            pure (PP.List [PP.bracketed_comma_list PP.Inconsistent clauses, " -> ", result, ";"])

        clause (BackendIR.MatchClause'Match b m) = refer_binding b >>= \ b -> matcher m >>= \ matcher -> pure (PP.List [b, " -> ", matcher])
        clause (BackendIR.MatchClause'Binding b) = define_binding b

        matcher (BackendIR.Match'BoolLiteral b) = pure $ if b then "true" else "false"
        matcher BackendIR.Match'Tuple = pure "(,)"
        matcher (BackendIR.Match'AnonADTVariant m_variant) =
            either
                (\ _ -> pure "<name resolution error>")
                (\ variant_index@(Type.ADT.VariantIndex _ adt_key _) ->
                    Type.PP.refer_adt <$> get_adt adt_key >>= \ adt_refer ->
                    Type.ADT.get_variant <$> get_adt_arena <*> pure variant_index >>= \ variant ->
                    let variant_name = Type.ADT.variant_name variant
                    in pure $ PP.List [adt_refer, " ", PP.String $ unlocate variant_name]
                )
                m_variant
expr (BackendIR.Expr'TupleDestructure1 _ _ other) = refer_binding other >>= \ other ->  pure (PP.List [other, ".0"])
expr (BackendIR.Expr'TupleDestructure2 _ _ other) = refer_binding other >>= \ other ->  pure (PP.List [other, ".1"])
expr (BackendIR.Expr'ADTDestructure _ _ base m_field_idx) =
    -- TODO: factor out referring to variant index into Type module?
    refer_binding base >>= \ base ->
    either
        (\ _ -> pure ("<error>", "<error>"))
        (\ (Type.ADT.FieldIndex _ variant_idx@(Type.ADT.VariantIndex _ adt_key _) field_idx) ->
            Type.PP.refer_adt <$> get_adt adt_key >>= \ adt_referred ->
            Type.ADT.get_variant <$> get_adt_arena <*> pure variant_idx >>= \ variant ->
            let variant_name = Type.ADT.variant_name variant
            in pure (PP.List [adt_referred, " ", PP.String $ unlocate variant_name], PP.String $ show field_idx)
        )
        m_field_idx >>= \ (variant_referred, field) ->
    pure (PP.List ["(", base, " as ", variant_referred, ").", field])
expr (BackendIR.Expr'Forall _ _ vars group e) = mapM type_var vars >>= \ vars -> define_binding_group group >>= \ group -> refer_binding e >>= \ e -> pure (PP.FirstOnLineIfMultiline $ PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent $ toList vars, " ", PP.indented_block [group, e]])
expr (BackendIR.Expr'TypeApply _ _ e arg) = refer_binding e >>= \ e -> refer_type arg >>= \ arg -> pure (PP.List [e, "#(", arg, ")"])
expr (BackendIR.Expr'MakeADT _ _ variant_index@(Type.ADT.VariantIndex _ adt_key _) tyargs args) =
    Type.PP.refer_adt <$> get_adt adt_key >>= \ adt_referred ->
    Type.ADT.get_variant <$> get_adt_arena <*> pure variant_index >>= \ variant ->
    mapM refer_binding args >>= \ args ->
    mapM refer_type tyargs >>= \ tyargs ->
    let variant_name = unlocate $ Type.ADT.variant_name variant
    in pure $ PP.List ["adt ", adt_referred, " ", PP.String variant_name, "#", PP.parenthesized_comma_list PP.Inconsistent tyargs, PP.bracketed_comma_list PP.Inconsistent args]
expr (BackendIR.Expr'Poison _ _ _) = pure $ PP.String "poison"
