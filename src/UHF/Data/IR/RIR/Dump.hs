module UHF.Data.IR.RIR.Dump (dump) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.DumpUtils as DumpUtils

import qualified UHF.Data.IR.RIR as RIR
import qualified UHF.Data.IR.Keys as Keys
import qualified UHF.Data.IR.Type as Type

-- TODO: dump types too

type Dumper captures = ReaderT (RIR.RIR captures) DumpUtils.Dumper

get_adt :: Keys.ADTKey -> Dumper captures (Type.ADT (Maybe (Type.Type Void)))
get_adt k = reader (\ (RIR.RIR _ adts _ _ _) -> Arena.get adts k)
get_type_synonym :: Keys.TypeSynonymKey -> Dumper captures (Type.TypeSynonym (Maybe (Type.Type Void)))
get_type_synonym k = reader (\ (RIR.RIR _ _ type_synonyms _ _) -> Arena.get type_synonyms k)

dump :: RIR.RIR captures -> Text
dump ir@(RIR.RIR decls _ _ _ mod) = DumpUtils.exec_dumper $ runReaderT (define_decl $ Arena.get decls mod) ir

text :: Text -> Dumper captures ()
text = lift . DumpUtils.dump

define_decl :: RIR.Decl captures -> Dumper captures ()
define_decl (RIR.Decl'Module bindings adts type_synonyms) = mapM_ define_adt adts >> mapM_ define_type_synonym type_synonyms >> mapM_ define_binding bindings
define_decl (RIR.Decl'Type _) = pure ()

-- TODO: move this to Type.Dump? because this is also duplicated across all the ir dumps
define_adt :: Type.ADTKey -> Dumper captures ()
define_adt k = get_adt k >>= \ (Type.ADT name _) -> text "data " >> text name >> text ";\n" -- TODO
define_type_synonym :: Type.TypeSynonymKey  -> Dumper captures ()
define_type_synonym k = get_type_synonym k >>= \ (Type.TypeSynonym name expansion) -> text "typesyn " >> text name >> text " = " >> refer_m_type expansion >> text ";\n"

refer_m_type :: Maybe (Type.Type Void) -> Dumper captures () -- TODO: remove
refer_m_type (Just ty) = refer_type ty
refer_m_type Nothing = text "<type error>"

refer_adt :: Type.ADTKey -> Dumper captures ()
refer_adt k = get_adt k >>= \ (Type.ADT name _) -> text name -- TODO: dump path
refer_type_synonym :: Type.TypeSynonymKey -> Dumper captures ()
refer_type_synonym k = get_type_synonym k >>= \ (Type.TypeSynonym name _) -> text name

refer_type :: Type.Type Void -> Dumper captures ()
refer_type (Type.Type'ADT k) = refer_adt k
refer_type (Type.Type'Synonym k) = refer_type_synonym k
refer_type Type.Type'Int = text "int"
refer_type Type.Type'Float = text "float"
refer_type Type.Type'Char = text "char"
refer_type Type.Type'String = text "string"
refer_type Type.Type'Bool = text "bool"
refer_type (Type.Type'Function a r) = refer_type a >> text " -> " >> refer_type r
refer_type (Type.Type'Tuple a b) = text "(" >> refer_type a >> text ", " >> refer_type b >> text ")"
refer_type (Type.Type'Variable void) = absurd void

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
expr (RIR.Expr'Float _ _ f) = text $ show f
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
