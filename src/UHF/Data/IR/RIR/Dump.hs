module UHF.Data.IR.RIR.Dump (dump) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.DumpUtils as DumpUtils

import qualified UHF.Data.IR.RIR as RIR
import qualified UHF.Data.IR.Keys as Keys
import qualified UHF.Data.IR.Type as Type

-- TODO: dont dump decls, just dump module
-- TODO: dump types too

type Dumper = ReaderT RIR.RIR DumpUtils.Dumper

get_adt :: Keys.ADTKey -> Dumper (Type.ADT (Maybe (Type.Type Void)))
get_adt k = reader (\ (RIR.RIR _ adts _ _) -> Arena.get adts k)
get_type_synonym :: Keys.TypeSynonymKey -> Dumper (Type.TypeSynonym (Maybe (Type.Type Void)))
get_type_synonym k = reader (\ (RIR.RIR _ _ type_synonyms _) -> Arena.get type_synonyms k)

dump :: RIR.RIR -> Text
dump ir@(RIR.RIR decls _ _ _) = DumpUtils.exec_dumper $ runReaderT (Arena.transformM dump_decl decls) ir

dump_text :: Text -> Dumper ()
dump_text = lift . DumpUtils.dump

dump_decl :: RIR.Decl -> Dumper ()
dump_decl (RIR.Decl'Module bindings adts type_synonyms) = mapM_ (\ k -> get_adt k >>= dump_adt) adts >> mapM_ (\ k -> get_type_synonym k >>= dump_type_synonym) type_synonyms >> mapM_ dump_binding bindings

dump_decl (RIR.Decl'Type ty) = pure ()

dump_adt :: Type.ADT (Maybe (Type.Type Void)) -> Dumper ()
dump_adt (Type.ADT name variants) = dump_text "data " >> dump_text name >> dump_text ";\n" -- TODO
dump_type_synonym :: Type.TypeSynonym (Maybe (Type.Type Void)) -> Dumper ()
dump_type_synonym (Type.TypeSynonym name expansion) = dump_text "typesyn " >> dump_text name >> dump_text " = " >> dump_m_type expansion >> dump_text ";\n"

dump_m_type :: Maybe (Type.Type Void) -> Dumper () -- TODO: remove
dump_m_type (Just ty) = dump_type ty
dump_m_type Nothing = dump_text "<type error>"

dump_type :: Type.Type Void -> Dumper ()
dump_type (Type.Type'ADT k) = get_adt k >>= \ (Type.ADT name _) -> dump_text name -- TODO: dump path
dump_type (Type.Type'Synonym k) = get_type_synonym k >>= \ (Type.TypeSynonym name _) -> dump_text name
dump_type (Type.Type'Int) = dump_text "int"
dump_type (Type.Type'Float) = dump_text "float"
dump_type (Type.Type'Char) = dump_text "char"
dump_type (Type.Type'String) = dump_text "string"
dump_type (Type.Type'Bool) = dump_text "bool"
dump_type (Type.Type'Function a r) = dump_type a >> dump_text " -> " >> dump_type r
dump_type (Type.Type'Tuple a b) = dump_text "(" >> dump_type a >> dump_text ", " >> dump_type b >> dump_text ")"
dump_type (Type.Type'Variable void) = absurd void

dump_binding :: RIR.Binding -> Dumper ()
dump_binding (RIR.Binding bvk expr) = dump_bvk bvk >> dump_text " = " >> dump_expr expr >> dump_text ";\n"

dump_bvk :: Keys.BoundValueKey -> Dumper ()
dump_bvk bvk = dump_text "_" >> dump_text (show $ Arena.unmake_key bvk) -- TODO: dont use unmake_key
dump_expr :: RIR.Expr -> Dumper ()
dump_expr (RIR.Expr'Identifier ty _ (Just bvk)) = dump_bvk bvk
dump_expr (RIR.Expr'Identifier ty _ Nothing) = dump_text "<name resolution error>"
dump_expr (RIR.Expr'Char ty _ c) = dump_text $ show c
dump_expr (RIR.Expr'String ty _ s) = dump_text $ show s
dump_expr (RIR.Expr'Int ty _ i) = dump_text $ show i
dump_expr (RIR.Expr'Float ty _ f) = dump_text $ show f
dump_expr (RIR.Expr'Bool ty _ b) = dump_text $ if b then "true" else "false"
dump_expr (RIR.Expr'Tuple ty _ a b) = dump_text "(" >> dump_expr a >> dump_text ", " >> dump_expr b >> dump_text ")"
dump_expr (RIR.Expr'Lambda ty _ param body) = dump_text "\\ " >> dump_bvk param >> dump_text " -> " >> dump_expr body
dump_expr (RIR.Expr'Let ty _ bindings res) = dump_text "let {\n" >> lift DumpUtils.indent >> mapM_ dump_binding bindings >> lift DumpUtils.dedent >> dump_text "}\n" >> dump_expr res
dump_expr (RIR.Expr'Call ty _ callee arg) = dump_expr callee >> dump_text "(" >> dump_expr arg >> dump_text ")"
dump_expr (RIR.Expr'Switch ty _ e arms) = dump_text "switch " >> dump_expr e >> dump_text " {\n" >> lift DumpUtils.indent >> mapM_ dump_arm arms >> lift DumpUtils.dedent >> dump_text "}"
    where
        dump_arm (RIR.Switch'BoolLiteral b, expr) = (if b then dump_text "true" else dump_text "false") >> dump_text " -> " >> dump_expr expr >> dump_text "\n"
        dump_arm (RIR.Switch'Tuple a b, expr) = dump_text "(" >> maybe (dump_text "_") dump_bvk a >> dump_text ", " >> maybe (dump_text "_") dump_bvk b >> dump_text ") -> " >> dump_expr expr >> dump_text "\n"
        dump_arm (RIR.Switch'Default, expr) = dump_text "_ -> " >> dump_expr expr >> dump_text "\n"
dump_expr (RIR.Expr'Poison ty _) = dump_text "poison"
