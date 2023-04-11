module UHF.Data.IR.RIR.PP (dump_main_module) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.PPUtils as PPUtils

import qualified UHF.Data.IR.RIR as RIR
import qualified UHF.Data.IR.Keys as Keys
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.PP as Type.PP
import qualified UHF.Data.IR.ID as ID

-- TODO: dump types too

type PP captures = ReaderT (RIR.RIR captures) PPUtils.PP

get_adt_arena :: PP captures (Arena.Arena (Type.ADT (Maybe (Type.Type Void))) Type.ADTKey)
get_adt_arena = reader (\ (RIR.RIR _ adts _ _ _ _) -> adts)
get_type_synonym_arena :: PP captures (Arena.Arena (Type.TypeSynonym (Maybe (Type.Type Void))) Type.TypeSynonymKey)
get_type_synonym_arena = reader (\ (RIR.RIR _ _ syns _ _ _) -> syns)
get_type_var_arena :: PP captures (Arena.Arena Type.Var Type.TypeVarKey)
get_type_var_arena = reader (\ (RIR.RIR _ _ _ vars _ _) -> vars)

get_adt :: Keys.ADTKey -> PP captures (Type.ADT (Maybe (Type.Type Void)))
get_adt k = reader (\ (RIR.RIR _ adts _ _ _ _) -> Arena.get adts k)
get_type_synonym :: Keys.TypeSynonymKey -> PP captures (Type.TypeSynonym (Maybe (Type.Type Void)))
get_type_synonym k = reader (\ (RIR.RIR _ _ type_synonyms _ _ _) -> Arena.get type_synonyms k)
get_bv :: Keys.BoundValueKey -> PP captures (RIR.BoundValue (Maybe (Type.Type Void)))
get_bv k = reader (\ (RIR.RIR _ _ _ _ bvs _) -> Arena.get bvs k)

dump_main_module :: RIR.RIR captures -> Text
dump_main_module ir@(RIR.RIR decls _ _ _ _ mod) = PPUtils.exec_pp $ runReaderT (define_decl $ Arena.get decls mod) ir

text :: Text -> PP captures ()
text = lift . PPUtils.write

define_decl :: RIR.Decl captures -> PP captures ()
define_decl (RIR.Decl'Module bindings adts type_synonyms) =
    ask >>= \ rir ->
    mapM_ (\ k -> get_adt k >>= lift . Type.PP.define_adt) adts >>
    mapM_ (\ k -> get_type_synonym k >>= lift . Type.PP.define_type_synonym (\ ty -> runReaderT (refer_m_type ty) rir)) type_synonyms >>
    mapM_ define_binding bindings
define_decl (RIR.Decl'Type _) = pure ()

refer_m_type :: Maybe (Type.Type Void) -> PP captures () -- TODO: remove
refer_m_type (Just ty) =
    get_adt_arena >>= \ adt_arena ->
    get_type_synonym_arena >>= \ type_synonym_arena ->
    get_type_var_arena >>= \ type_var_arena ->
    lift (Type.PP.refer_type absurd adt_arena type_synonym_arena type_var_arena ty)
refer_m_type Nothing = text "<type error>"

define_binding :: RIR.Binding captures -> PP captures ()
define_binding (RIR.Binding bvk e) =
    let name = refer_bv bvk
        initializer = expr e
    in ask >>= \ ir -> if PPUtils.is_multiline (runReaderT initializer ir)
        then name >> text " =\n" >> lift PPUtils.indent >> initializer >> text "\n" >> lift PPUtils.dedent >> text ";\n"
        else name >> text " = " >> initializer >> text ";\n"

refer_bv :: Keys.BoundValueKey -> PP captures ()
refer_bv bvk = get_bv bvk >>= \ (RIR.BoundValue id _ _ _) -> text (ID.stringify id)

-- TODO: properly handle when exprs are multiline
expr :: RIR.Expr captures -> PP captures ()
expr (RIR.Expr'Identifier _ _ _ (Just bvk)) = refer_bv bvk
expr (RIR.Expr'Identifier _ _ _ Nothing) = text "<name resolution error>"
expr (RIR.Expr'Char _ _ _ c) = text $ show c
expr (RIR.Expr'String _ _ _ s) = text $ show s
expr (RIR.Expr'Int _ _ _ i) = text $ show i
expr (RIR.Expr'Float _ _ _ (n :% d)) = text $ "(" <> show n <> "/" <> show d <> ")"
expr (RIR.Expr'Bool _ _ _ b) = text $ if b then "true" else "false"
expr (RIR.Expr'Tuple _ _ _ a b) = text "(" >> expr a >> text ", " >> expr b >> text ")"
expr (RIR.Expr'Lambda _ _ _ _ _ param body) = text "\\ " >> refer_bv param >> text " -> " >> expr body -- TODO: show captures
expr (RIR.Expr'Let _ _ _ [] res) = text "let {}\n" >> expr res
expr (RIR.Expr'Let _ _ _ [binding] res) = text "let " >> define_binding binding >> expr res
expr (RIR.Expr'Let _ _ _ bindings res) = text "let {\n" >> lift PPUtils.indent >> mapM_ define_binding bindings >> lift PPUtils.dedent >> text "}\n" >> expr res
expr (RIR.Expr'Call _ _ _ callee arg) = expr callee >> text "(" >> expr arg >> text ")"
expr (RIR.Expr'Switch _ _ _ e arms) = text "switch " >> expr e >> text " {\n" >> lift PPUtils.indent >> mapM_ pp_arm arms >> lift PPUtils.dedent >> text "}"
    where
        -- TODO: properly indent these if the expression is multiline
        pp_arm (RIR.Switch'BoolLiteral b, e) = (if b then text "true" else text "false") >> text " -> " >> expr e >> text "\n"
        pp_arm (RIR.Switch'Tuple a b, e) = text "(" >> maybe (text "_") refer_bv a >> text ", " >> maybe (text "_") refer_bv b >> text ") -> " >> expr e >> text "\n"
        pp_arm (RIR.Switch'Default, e) = text "_ -> " >> expr e >> text "\n"
expr (RIR.Expr'Seq _ _ _ a b) = text "seq " >> expr a >> text ", " >> expr b
expr (RIR.Expr'Forall _ _ _ _ _) = todo
expr (RIR.Expr'TypeApply _ _ _ _ _) = todo
expr (RIR.Expr'MakeADT _ _ _ _ _) = todo
expr (RIR.Expr'Poison _ _ _) = text "poison"
