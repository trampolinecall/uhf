module UHF.Data.IR.RIR.Dump (dump) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.DumpUtils as DumpUtils

import qualified UHF.Data.IR.RIR as RIR
import qualified UHF.Data.IR.Keys as Keys
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.Dump as Type.Dump

-- TODO: dump types too

type Dumper captures = ReaderT (RIR.RIR captures) DumpUtils.Dumper

get_adt_arena :: Dumper captures (Arena.Arena (Type.ADT (Maybe (Type.Type Void))) Type.ADTKey)
get_adt_arena = reader (\ (RIR.RIR _ adts _ _ _) -> adts)
get_type_synonym_arena :: Dumper captures (Arena.Arena (Type.TypeSynonym (Maybe (Type.Type Void))) Type.TypeSynonymKey)
get_type_synonym_arena = reader (\ (RIR.RIR _ _ syns _ _) -> syns)

get_adt :: Keys.ADTKey -> Dumper captures (Type.ADT (Maybe (Type.Type Void)))
get_adt k = reader (\ (RIR.RIR _ adts _ _ _) -> Arena.get adts k)
get_type_synonym :: Keys.TypeSynonymKey -> Dumper captures (Type.TypeSynonym (Maybe (Type.Type Void)))
get_type_synonym k = reader (\ (RIR.RIR _ _ type_synonyms _ _) -> Arena.get type_synonyms k)

dump :: RIR.RIR captures -> Text
dump ir@(RIR.RIR decls _ _ _ mod) = DumpUtils.exec_dumper $ runReaderT (define_decl $ Arena.get decls mod) ir

text :: Text -> Dumper captures ()
text = lift . DumpUtils.dump

define_decl :: RIR.Decl captures -> Dumper captures ()
define_decl (RIR.Decl'Module bindings adts type_synonyms) =
    ask >>= \ rir ->
    get_adt_arena >>= \ adt_arena ->
    get_type_synonym_arena >>= \ type_synonym_arena ->
    mapM_ (lift . Type.Dump.define_adt adt_arena) adts >> mapM_ (lift . Type.Dump.define_type_synonym (\ ty -> runReaderT (refer_m_type ty) rir) type_synonym_arena) type_synonyms >> mapM_ define_binding bindings
define_decl (RIR.Decl'Type _) = pure ()

refer_m_type :: Maybe (Type.Type Void) -> Dumper captures () -- TODO: remove
refer_m_type (Just ty) =
    get_adt_arena >>= \ adt_arena ->
    get_type_synonym_arena >>= \ type_synonym_arena ->
    lift (Type.Dump.refer_type adt_arena type_synonym_arena ty)
refer_m_type Nothing = text "<type error>"

define_binding :: RIR.Binding captures -> Dumper captures ()
define_binding (RIR.Binding bvk e) =
    let name = refer_bv bvk
        initializer = expr e
    in ask >>= \ ir -> if DumpUtils.is_multiline (runReaderT initializer ir)
        then name >> text " = \n" >> lift DumpUtils.indent >> initializer >> text "\n" >> lift DumpUtils.dedent >> text ";\n"
        else name >> text " = " >> initializer >> text ";\n"

-- TODO: properly handle when exprs are multiline
refer_bv :: Keys.BoundValueKey -> Dumper captures ()
refer_bv bvk = text "_" >> text (show $ Arena.unmake_key bvk) -- TODO: dont use unmake_key

expr :: RIR.Expr captures -> Dumper captures ()
expr (RIR.Expr'Identifier _ _ (Just bvk)) = refer_bv bvk
expr (RIR.Expr'Identifier _ _ Nothing) = text "<name resolution error>"
expr (RIR.Expr'Char _ _ c) = text $ show c
expr (RIR.Expr'String _ _ s) = text $ show s
expr (RIR.Expr'Int _ _ i) = text $ show i
expr (RIR.Expr'Float _ _ (n :% d)) = text $ "(" <> show n <> "/" <> show d <> ")"
expr (RIR.Expr'Bool _ _ b) = text $ if b then "true" else "false"
expr (RIR.Expr'Tuple _ _ a b) = text "(" >> expr a >> text ", " >> expr b >> text ")"
expr (RIR.Expr'Lambda _ _ _ _ param body) = text "\\ " >> refer_bv param >> text " -> " >> expr body -- TODO: show captures
expr (RIR.Expr'Let _ _ bindings res) = text "let {\n" >> lift DumpUtils.indent >> mapM_ define_binding bindings >> lift DumpUtils.dedent >> text "}\n" >> expr res
expr (RIR.Expr'Call _ _ callee arg) = expr callee >> text "(" >> expr arg >> text ")"
expr (RIR.Expr'Switch _ _ e arms) = text "switch " >> expr e >> text " {\n" >> lift DumpUtils.indent >> mapM_ dump_arm arms >> lift DumpUtils.dedent >> text "}"
    where
        -- TODO: properly indent these if the expression is multiline
        dump_arm (RIR.Switch'BoolLiteral b, e) = (if b then text "true" else text "false") >> text " -> " >> expr e >> text "\n"
        dump_arm (RIR.Switch'Tuple a b, e) = text "(" >> maybe (text "_") refer_bv a >> text ", " >> maybe (text "_") refer_bv b >> text ") -> " >> expr e >> text "\n"
        dump_arm (RIR.Switch'Default, e) = text "_ -> " >> expr e >> text "\n"
expr (RIR.Expr'Seq _ _ a b) = text "seq " >> expr a >> text ", " >> expr b
expr (RIR.Expr'Poison _ _) = text "poison"
