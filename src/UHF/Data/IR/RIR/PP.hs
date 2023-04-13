module UHF.Data.IR.RIR.PP (dump_main_module) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.PP as PP

import qualified UHF.Data.IR.RIR as RIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.PP as Type.PP
import qualified UHF.Data.IR.ID as ID

-- TODO: dump types too

type IRReader captures = Reader (RIR.RIR captures)

get_adt_arena :: IRReader captures (Arena.Arena (Type.ADT (Maybe (Type.Type Void))) Type.ADTKey)
get_adt_arena = reader (\ (RIR.RIR _ adts _ _ _ _) -> adts)
get_type_synonym_arena :: IRReader captures (Arena.Arena (Type.TypeSynonym (Maybe (Type.Type Void))) Type.TypeSynonymKey)
get_type_synonym_arena = reader (\ (RIR.RIR _ _ syns _ _ _) -> syns)
get_type_var_arena :: IRReader captures (Arena.Arena Type.Var Type.TypeVarKey)
get_type_var_arena = reader (\ (RIR.RIR _ _ _ vars _ _) -> vars)

get_adt :: Type.ADTKey -> IRReader captures (Type.ADT (Maybe (Type.Type Void)))
get_adt k = reader (\ (RIR.RIR _ adts _ _ _ _) -> Arena.get adts k)
get_type_synonym :: Type.TypeSynonymKey -> IRReader captures (Type.TypeSynonym (Maybe (Type.Type Void)))
get_type_synonym k = reader (\ (RIR.RIR _ _ type_synonyms _ _ _) -> Arena.get type_synonyms k)
get_bv :: RIR.BoundValueKey -> IRReader captures (RIR.BoundValue (Maybe (Type.Type Void)))
get_bv k = reader (\ (RIR.RIR _ _ _ _ bvs _) -> Arena.get bvs k)
get_type_var :: Type.TypeVarKey -> IRReader captures Type.Var
get_type_var k = reader (\ (RIR.RIR _ _ _ type_vars _ _) -> Arena.get type_vars k)

dump_main_module :: RIR.RIR captures -> Text
dump_main_module ir@(RIR.RIR decls _ _ _ _ mod) = PP.render $ runReader (define_decl $ Arena.get decls mod) ir

define_decl :: RIR.Decl captures -> IRReader captures PP.Token
define_decl (RIR.Decl'Module bindings adts type_synonyms) =
    ask >>= \ rir ->
    mapM (fmap Type.PP.define_adt . get_adt) adts >>= \ adts ->
    mapM (fmap (Type.PP.define_type_synonym (\ ty -> runReader (refer_m_type ty) rir)) . get_type_synonym) type_synonyms >>= \ type_synonyms ->
    mapM define_binding bindings >>= \ bindings ->
    pure (PP.flat_block $ adts <> type_synonyms <> bindings)
define_decl (RIR.Decl'Type _) = pure $ PP.List []

refer_m_type :: Maybe (Type.Type Void) -> IRReader captures PP.Token -- TODO: remove
refer_m_type (Just ty) =
    get_adt_arena >>= \ adt_arena ->
    get_type_synonym_arena >>= \ type_synonym_arena ->
    get_type_var_arena >>= \ type_var_arena ->
    pure (Type.PP.refer_type absurd adt_arena type_synonym_arena type_var_arena ty)
refer_m_type Nothing = pure "<type error>"

define_binding :: RIR.Binding captures -> IRReader captures PP.Token
define_binding (RIR.Binding bvk e) =
    refer_bv bvk >>= \ bvk ->
    expr e >>= \ e ->
    pure (PP.List [bvk, " = ", e, ";"])

refer_bv :: RIR.BoundValueKey -> IRReader captures PP.Token
refer_bv bvk = get_bv bvk >>= \ (RIR.BoundValue id _ _ _) -> pure (PP.String (ID.stringify id))

type_var :: Type.TypeVarKey -> IRReader captures PP.Token
type_var k = get_type_var k >>= \ (Type.Var name) -> pure (PP.String name)

expr :: RIR.Expr captures -> IRReader captures PP.Token
expr (RIR.Expr'Identifier _ _ _ (Just bvk)) = refer_bv bvk
expr (RIR.Expr'Identifier _ _ _ Nothing) = pure $ PP.List ["<name resolution error>"]
expr (RIR.Expr'Char _ _ _ c) = pure $ PP.FirstOnLineIfMultiline $ PP.String $ show c
expr (RIR.Expr'String _ _ _ s) = pure $ PP.FirstOnLineIfMultiline $ PP.String $ show s
expr (RIR.Expr'Int _ _ _ i) = pure $ PP.FirstOnLineIfMultiline $ PP.String $ show i
expr (RIR.Expr'Float _ _ _ (n :% d)) = pure $ PP.FirstOnLineIfMultiline $ PP.String $ "(" <> show n <> "/" <> show d <> ")"
expr (RIR.Expr'Bool _ _ _ b) = pure $ PP.String $ if b then "true" else "false"
expr (RIR.Expr'Tuple _ _ _ a b) = expr a >>= \ a -> expr b >>= \ b -> pure (PP.parenthesized_comma_list [a, b])
expr (RIR.Expr'Lambda _ _ _ _ _ param body) = refer_bv param >>= \ param -> expr body >>= \ body -> pure (PP.FirstOnLineIfMultiline $ PP.List ["\\ ", param, " -> ", body]) -- TODO: show captures
expr (RIR.Expr'Let _ _ _ [binding] res) = define_binding binding >>= \ binding -> expr res >>= \ res -> pure (PP.FirstOnLineIfMultiline $ PP.List ["let ", binding, "\n", res])
expr (RIR.Expr'Let _ _ _ bindings res) = expr res >>= \ res -> mapM define_binding bindings >>= \ bindings -> pure (PP.FirstOnLineIfMultiline $ PP.List ["let ", PP.braced_block bindings, "\n", res])
expr (RIR.Expr'Call _ _ _ callee arg) = expr callee >>= \ callee -> expr arg >>= \ arg -> pure $ PP.List [callee, "(", arg, ")"]
expr (RIR.Expr'Switch _ _ _ e arms) = mapM pp_arm arms >>= \ arms -> expr e >>= \ e -> pure (PP.List ["switch ", e, " ", PP.braced_block arms])
    where
        pp_arm (RIR.Switch'BoolLiteral b, e) = expr e >>= \ e -> pure (PP.List [if b then "true" else "false", " -> ", e, ";"])
        pp_arm (RIR.Switch'Tuple a b, e) = maybe (pure "_") refer_bv a >>= \ a -> maybe (pure "_") refer_bv b >>= \ b -> expr e >>= \ e -> pure (PP.List ["(", a, ", ", b, ") -> ", e, ";"])
        pp_arm (RIR.Switch'Default, e) = expr e >>= \ e -> pure (PP.List ["_ -> ", e, ";"])
expr (RIR.Expr'Seq _ _ _ a b) = expr a >>= \ a -> expr b >>= \ b -> pure $ PP.FirstOnLineIfMultiline $ PP.List ["seq ", a, ", ", b]
expr (RIR.Expr'Forall _ _ _ tys e) = mapM type_var tys >>= \ tys -> expr e >>= \ e -> pure (PP.List ["#", PP.parenthesized_comma_list $ toList tys, " ", e])
expr (RIR.Expr'TypeApply _ _ _ e arg) = expr e >>= \ e -> refer_m_type arg >>= \ arg -> pure (PP.List [e, "#(", arg, ")"])
expr (RIR.Expr'MakeADT _ _ _ variant_index@(Type.ADTVariantIndex adt_key _) args) =
    Type.PP.refer_adt <$> get_adt adt_key >>= \ adt_refer ->
    Type.get_adt_variant <$> get_adt_arena <*> pure variant_index >>= \ variant ->
    mapM expr args >>= \ args ->
    let variant_name = Type.variant_name variant
    in pure $ PP.FirstOnLineIfMultiline $ PP.List ["adt ", adt_refer, " ", PP.String variant_name, PP.bracketed_comma_list args]
expr (RIR.Expr'Poison _ _ _) = pure $ PP.List ["poison"]
