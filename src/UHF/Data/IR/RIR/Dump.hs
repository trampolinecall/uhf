module UHF.Data.IR.RIR.Dump (dump) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.DumpUtils as DumpUtils

import qualified UHF.Data.IR.HIR as HIR
import qualified UHF.Data.IR.RIR as RIR
import qualified UHF.Data.IR.Keys as Keys
import qualified UHF.Data.IR.Type as Type

-- TODO: dont dump decls, just dump module
-- TODO: dump types too

type IR = (Arena.Arena RIR.Decl Keys.DeclKey, Arena.Arena (HIR.ADT (Maybe (Type.Type Void))) Keys.ADTKey, Arena.Arena (HIR.TypeSynonym (Maybe (Type.Type Void))) Keys.TypeSynonymKey, Arena.Arena (HIR.BoundValue (Maybe (Type.Type Void))) Keys.BoundValueKey)
type Dumper = ReaderT IR DumpUtils.Dumper

dump :: IR -> Text
dump ir@(decls, _, _, _) = DumpUtils.exec_dumper $ runReaderT (Arena.transformM dump_decl decls) ir

dump_text :: Text -> Dumper ()
dump_text = lift . DumpUtils.dump

dump_decl :: RIR.Decl -> Dumper ()
dump_decl (RIR.Decl'Module bindings) = mapM_ dump_binding bindings
dump_decl (RIR.Decl'Type ty) = pure ()

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
