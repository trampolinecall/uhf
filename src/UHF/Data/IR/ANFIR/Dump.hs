module UHF.Data.IR.ANFIR.Dump (dump) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.DumpUtils as DumpUtils

import qualified UHF.Data.IR.ANFIR as ANFIR
import qualified UHF.Data.IR.Keys as Keys
import qualified UHF.Data.IR.Type as Type

-- TODO: dont dump decls, just dump module
-- TODO: dump types too

type Dumper ty poison_allowed = ReaderT (ANFIR.ANFIR ty poison_allowed) DumpUtils.Dumper

get_binding :: Keys.BindingKey -> Dumper ty poison_allowed (ANFIR.Binding ty poison_allowed)
get_binding k = reader (\ (ANFIR.ANFIR _ _ _ bindings _) -> Arena.get bindings k)

dump :: ANFIR.ANFIR ty poison_allowed -> Text
dump ir@(ANFIR.ANFIR decls adts type_synonyms bindings params) = DumpUtils.exec_dumper $ runReaderT (Arena.transformM dump_decl decls) ir

dump_text :: Text -> Dumper ty poison_allowed ()
dump_text = lift . DumpUtils.dump

dump_decl :: ANFIR.Decl -> Dumper ty poison_allowed ()
dump_decl (ANFIR.Decl'Module bindings) = mapM_ (\ k -> get_binding k >>= dump_binding k) bindings
dump_decl (ANFIR.Decl'Type ty) = pure ()

dump_binding_key :: ANFIR.BindingKey -> Dumper ty poison_allowed ()
dump_binding_key key = dump_text ("_" <> show (Arena.unmake_key key))
dump_param_key :: ANFIR.ParamKey -> Dumper ty poison_allowed ()
dump_param_key key = dump_text ("p_" <> show (Arena.unmake_key key))

dump_binding :: ANFIR.BindingKey -> ANFIR.Binding ty poison_allowed -> Dumper ty poison_allowed ()
dump_binding key (ANFIR.Binding _ expr) = dump_binding_key key >> dump_text " = " >> dump_expr expr >> dump_text ";\n" -- TODO: dont use unmake_key

dump_expr :: ANFIR.Expr ty poison_allowed -> Dumper ty poison_allowed ()
dump_expr (ANFIR.Expr'Identifier ty bk) = dump_binding_key bk
dump_expr (ANFIR.Expr'Int ty i) = dump_text $ show i
dump_expr (ANFIR.Expr'Float ty f) = dump_text $ show f
dump_expr (ANFIR.Expr'Bool ty b) = dump_text $ if b then "true" else "false"
dump_expr (ANFIR.Expr'Char ty c) = dump_text $ show c
dump_expr (ANFIR.Expr'String ty s) = dump_text $ show s
dump_expr (ANFIR.Expr'Tuple ty a b) = dump_text "(" >> dump_binding_key a >> dump_text ", " >> dump_binding_key b >> dump_text ")"
dump_expr (ANFIR.Expr'Lambda ty param bindings body) = dump_text "\\ " >> dump_param_key param >> dump_text " -> {\n" >> lift DumpUtils.indent >> mapM_ (\ k -> get_binding k >>= dump_binding k) bindings >> lift DumpUtils.dedent >> dump_text "}\n" >> dump_binding_key body
dump_expr (ANFIR.Expr'Param ty pk) = dump_param_key pk
dump_expr (ANFIR.Expr'Call ty callee arg) = dump_binding_key callee >> dump_text "(" >> dump_binding_key arg >> dump_text ")"
dump_expr (ANFIR.Expr'Switch ty e arms) = dump_text "switch " >> dump_binding_key e >> dump_text " {\n" >> lift DumpUtils.indent >> mapM_ dump_arm arms >> lift DumpUtils.dedent >> dump_text "}"
    where
        dump_arm (ANFIR.Switch'BoolLiteral b, expr) = (if b then dump_text "true" else dump_text "false") >> dump_text " -> " >> dump_binding_key expr >> dump_text "\n"
        dump_arm (ANFIR.Switch'Tuple, expr) = dump_text "(,) -> " >> dump_binding_key expr >> dump_text "\n"
        dump_arm (ANFIR.Switch'Default, expr) = dump_text "-> " >> dump_binding_key expr >> dump_text "\n"
dump_expr (ANFIR.Expr'TupleDestructure1 ty other) = dump_binding_key other >> dump_text ".0"
dump_expr (ANFIR.Expr'TupleDestructure2 ty other) = dump_binding_key other >> dump_text ".1"
dump_expr (ANFIR.Expr'Poison ty _) = dump_text "poison"
