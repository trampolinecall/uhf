module UHF.Data.IR.RIR.PP (dump_cu) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.PP as PP

import qualified UHF.Data.IR.RIR as RIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.PP as Type.PP
import qualified UHF.Data.IR.ID as ID

import UHF.IO.Located (Located (Located, unlocate))

-- TODO: dump types too

type IRReader = Reader RIR.RIR

get_adt_arena :: IRReader (Arena.Arena (Type.ADT (Maybe (Type.Type Void))) Type.ADTKey)
get_adt_arena = reader (\ (RIR.RIR adts _ _ _ _) -> adts)
get_type_synonym_arena :: IRReader (Arena.Arena (Type.TypeSynonym (Maybe (Type.Type Void))) Type.TypeSynonymKey)
get_type_synonym_arena = reader (\ (RIR.RIR _ syns _ _ _) -> syns)
get_type_var_arena :: IRReader (Arena.Arena Type.Var Type.TypeVarKey)
get_type_var_arena = reader (\ (RIR.RIR _ _ vars _ _) -> vars)

get_adt :: Type.ADTKey -> IRReader (Type.ADT (Maybe (Type.Type Void)))
get_adt k = reader (\ (RIR.RIR adts _ _ _ _) -> Arena.get adts k)
get_type_synonym :: Type.TypeSynonymKey -> IRReader (Type.TypeSynonym (Maybe (Type.Type Void)))
get_type_synonym k = reader (\ (RIR.RIR _ type_synonyms _ _ _) -> Arena.get type_synonyms k)
get_bv :: RIR.BoundValueKey -> IRReader RIR.BoundValue
get_bv k = reader (\ (RIR.RIR _ _ _ bvs _) -> Arena.get bvs k)
get_type_var :: Type.TypeVarKey -> IRReader Type.Var
get_type_var k = reader (\ (RIR.RIR _ _ type_vars _ _) -> Arena.get type_vars k)

dump_cu :: RIR.RIR -> Text
dump_cu ir@(RIR.RIR _ _ _ _ cu) = PP.render $ runReader (define_cu cu) ir

define_cu :: RIR.CU -> IRReader PP.Token
define_cu (RIR.CU bindings adts type_synonyms) =
    ask >>= \ rir ->
    mapM (fmap Type.PP.define_adt . get_adt) adts >>= \ adts ->
    mapM (fmap (Type.PP.define_type_synonym (\ ty -> runReader (refer_m_type ty) rir)) . get_type_synonym) type_synonyms >>= \ type_synonyms ->
    mapM define_binding bindings >>= \ bindings ->
    pure (PP.flat_block $ adts <> type_synonyms <> bindings)

refer_m_type :: Maybe (Type.Type Void) -> IRReader PP.Token -- TODO: remove
refer_m_type (Just ty) =
    get_adt_arena >>= \ adt_arena ->
    get_type_synonym_arena >>= \ type_synonym_arena ->
    get_type_var_arena >>= \ type_var_arena ->
    pure (Type.PP.refer_type absurd adt_arena type_synonym_arena type_var_arena ty)
refer_m_type Nothing = pure "<type error>"

define_binding :: RIR.Binding -> IRReader PP.Token
define_binding (RIR.Binding bvk e) =
    refer_bv bvk >>= \ bvk ->
    expr e >>= \ e ->
    pure (PP.List [bvk, " = ", e, ";"])

refer_bv :: RIR.BoundValueKey -> IRReader PP.Token
refer_bv bvk = get_bv bvk >>= \ (RIR.BoundValue id _ _) -> pure (PP.String (ID.stringify id))

type_var :: Type.TypeVarKey -> IRReader PP.Token
type_var k = get_type_var k >>= \ (Type.Var (Located _ name)) -> pure (PP.String name)

-- TODO: precedence
expr :: RIR.Expr -> IRReader PP.Token
expr (RIR.Expr'Identifier _ _ _ (Just bvk)) = refer_bv bvk
expr (RIR.Expr'Identifier _ _ _ Nothing) = pure $ PP.List ["<name resolution error>"]
expr (RIR.Expr'Char _ _ c) = pure $ PP.FirstOnLineIfMultiline $ PP.String $ show c
expr (RIR.Expr'String _ _ s) = pure $ PP.FirstOnLineIfMultiline $ PP.String $ show s
expr (RIR.Expr'Int _ _ i) = pure $ PP.FirstOnLineIfMultiline $ PP.String $ show i
expr (RIR.Expr'Float _ _ (n :% d)) = pure $ PP.FirstOnLineIfMultiline $ PP.String $ "(" <> show n <> "/" <> show d <> ")"
expr (RIR.Expr'Bool _ _ b) = pure $ PP.String $ if b then "true" else "false"
expr (RIR.Expr'Tuple _ _ a b) = expr a >>= \ a -> expr b >>= \ b -> pure (PP.parenthesized_comma_list PP.Inconsistent [a, b])
expr (RIR.Expr'Lambda _ _ param body) = refer_bv param >>= \ param -> expr body >>= \ body -> pure (PP.FirstOnLineIfMultiline $ PP.List ["\\ ", param, " -> ", body])
expr (RIR.Expr'Let _ _ [binding] res) = define_binding binding >>= \ binding -> expr res >>= \ res -> pure (PP.FirstOnLineIfMultiline $ PP.List ["let ", binding, "\n", res])
expr (RIR.Expr'Let _ _ bindings res) = expr res >>= \ res -> mapM define_binding bindings >>= \ bindings -> pure (PP.FirstOnLineIfMultiline $ PP.List ["let ", PP.braced_block bindings, "\n", res])
expr (RIR.Expr'Call _ _ callee arg) = expr callee >>= \ callee -> expr arg >>= \ arg -> pure $ PP.List [callee, "(", arg, ")"]
expr (RIR.Expr'Match _ _ _ tree) = pp_tree tree >>= \ tree -> pure (PP.List ["match ", tree])
    where
        pp_tree (RIR.MatchTree arms) = mapM pp_arm arms >>= \ arms -> pure (PP.braced_block arms)

        pp_arm (clauses, result) =
            mapM pp_clause clauses >>= \ clauses ->
            (case result of
                Right e -> expr e
                Left subtree -> pp_tree subtree) >>= \ result ->
            pure (PP.List [PP.bracketed_comma_list PP.Inconsistent clauses, " -> ", result, ";"])

        pp_clause (RIR.MatchClause'Match bv matcher) = refer_bv bv >>= \ bv -> pp_matcher matcher >>= \ matcher -> pure (PP.List [bv, " -> ", matcher])
        pp_clause (RIR.MatchClause'Assign target rhs) = refer_bv target >>= \ target -> pp_assign_rhs rhs >>= \ rhs -> pure (PP.List [target, " = ", rhs])

        pp_matcher (RIR.Match'BoolLiteral b) = pure $ if b then "true" else "false"
        pp_matcher (RIR.Match'Tuple) = pure "(,)"
        pp_matcher (RIR.Match'AnonADTVariant m_variant) =
            maybe
                (pure "<name resolution error>")
                (\ variant_index@(Type.ADTVariantIndex adt_key _) ->
                    Type.PP.refer_adt <$> get_adt adt_key >>= \ adt_refer ->
                    Type.get_adt_variant <$> get_adt_arena <*> pure variant_index >>= \ variant ->
                    let variant_name = Type.variant_name variant
                    in pure $ PP.List [adt_refer, " ", PP.String $ unlocate variant_name]
                )
                m_variant

        pp_assign_rhs (RIR.MatchAssignRHS'OtherBVK other) = refer_bv other
        pp_assign_rhs (RIR.MatchAssignRHS'TupleDestructure1 _ tup) = refer_bv tup >>= \ tup -> pure (PP.List [tup, ".tuple_l"])
        pp_assign_rhs (RIR.MatchAssignRHS'TupleDestructure2 _ tup) = refer_bv tup >>= \ tup -> pure (PP.List [tup, ".tuple_r"])
        pp_assign_rhs (RIR.MatchAssignRHS'AnonADTVariantField _ base m_field) =
            refer_bv base >>= \ base ->
            -- TODO: unduplicate this?
            maybe
                (pure ("<error>", "<error>"))
                (\ (Type.ADTFieldIndex variant_idx@(Type.ADTVariantIndex adt_key _) field_idx) ->
                    Type.PP.refer_adt <$> get_adt adt_key >>= \ adt_refer ->
                    Type.get_adt_variant <$> get_adt_arena <*> pure variant_idx >>= \ variant ->
                    let variant_name = Type.variant_name variant
                    in pure $ (PP.List [adt_refer, " ", PP.String $ unlocate variant_name], PP.String $ show field_idx)
                )
                m_field >>= \ (refer_variant, field_idx) ->
            pure (PP.List ["(", base, " as ", refer_variant, ").", field_idx])

expr (RIR.Expr'Forall _ _ tys e) = mapM type_var tys >>= \ tys -> expr e >>= \ e -> pure (PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent $ toList tys, " ", e])
expr (RIR.Expr'TypeApply _ _ _ e arg) = expr e >>= \ e -> refer_m_type arg >>= \ arg -> pure (PP.List [e, "#(", arg, ")"])
expr (RIR.Expr'MakeADT _ _ variant_index@(Type.ADTVariantIndex adt_key _) tyargs args) =
    Type.PP.refer_adt <$> get_adt adt_key >>= \ adt_refer ->
    Type.get_adt_variant <$> get_adt_arena <*> pure variant_index >>= \ variant ->
    mapM expr args >>= \ args ->
    mapM refer_m_type tyargs >>= \ tyargs ->
    let variant_name = Type.variant_name variant
    in pure $ PP.FirstOnLineIfMultiline $ PP.List ["adt ", adt_refer, " ", PP.String $ unlocate variant_name, "#", PP.parenthesized_comma_list PP.Inconsistent tyargs, PP.bracketed_comma_list PP.Inconsistent args]
expr (RIR.Expr'Poison _ _ _) = pure $ PP.List ["poison"]
