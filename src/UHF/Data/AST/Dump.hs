{-# LANGUAGE FlexibleInstances #-}

module UHF.Data.AST.Dump (dump) where

import UHF.Util.Prelude

import qualified UHF.DumpUtils as DumpUtils
import qualified UHF.Data.AST as AST

import UHF.IO.Located (Located (..))

import qualified Data.Text as Text

dump :: [AST.Decl] -> Text
dump = DumpUtils.exec_dumper . dump_

class Dumpable d where
    dump_ :: d -> DumpUtils.Dumper ()

instance Dumpable [AST.Decl] where
    dump_ = mapM_ (\ decl -> dump_ decl >> DumpUtils.newline)

instance Dumpable AST.Decl where
    dump_ (AST.Decl'Value target _ init) = dump_struct "Decl'Value" [("target", dump_ target), ("init", dump_ init)]
    dump_ (AST.Decl'Data name variants) = dump_struct "Decl'Data" [("name", dump_ name), ("variants", dump_list dump_ variants)]
    dump_ (AST.Decl'TypeSyn name ty) = dump_struct "Decl'TypeSyn" [("name", dump_ name), ("ty", dump_ ty)]

instance Dumpable AST.DataVariant where
    dump_ (AST.DataVariant'Anon name fields) = dump_struct "DataVariant'Anon" [("name", dump_ name), ("fields", dump_list dump_ fields)]
    dump_ (AST.DataVariant'Named name fields) = dump_struct "DataVariant'Named" [("name", dump_ name), ("fields", dump_list (\ (name, ty) -> dump_ name >> DumpUtils.dump ": " >> dump_ ty) fields)]

instance Dumpable AST.Type where
    dump_ (AST.Type'Identifier iden) = dump_struct "Type'Identifier" [("iden", dump_ iden)]
    dump_ (AST.Type'Tuple _ items) = dump_struct "Type'Tuple" [("items", dump_list dump_ items)]

instance Dumpable AST.Expr where
    dump_ (AST.Expr'Identifier iden) = dump_struct "Expr'Identifier" [("iden", dump_ iden)]
    dump_ (AST.Expr'Char _ c) = dump_struct "Expr'Char" [("c", DumpUtils.dump $ show c)]
    dump_ (AST.Expr'String _ s) = dump_struct "Expr'String" [("s", DumpUtils.dump $ show s)]
    dump_ (AST.Expr'Int _ i) = dump_struct "Expr'Int" [("i", DumpUtils.dump $ show i)]
    dump_ (AST.Expr'Float _ f) = dump_struct "Expr'Float" [("f", DumpUtils.dump $ show f)]
    dump_ (AST.Expr'Bool _ b) = dump_struct "Expr'Bool" [("b", DumpUtils.dump $ if b then "true" else "false")]
    dump_ (AST.Expr'Tuple _ items) = dump_struct "Expr'Tuple" [("items", dump_list dump_ items)]
    dump_ (AST.Expr'Lambda _ args body) = dump_struct "Expr'Lambda" [("args", dump_list dump_ args), ("body", dump_ body)]
    dump_ (AST.Expr'Let _ decls res) = dump_struct "Expr'Let" [("decls", dump_ decls), ("res", dump_ res)]
    dump_ (AST.Expr'LetRec _ decls res) = dump_struct "Expr'LetRec" [("decls", dump_ decls), ("res", dump_ res)]
    dump_ (AST.Expr'BinaryOps _ first ops) = dump_struct "Expr'BinaryOps" [("first", dump_ first), ("ops", dump_list (\ (op, rhs) -> dump_ op >> DumpUtils.dump " " >> dump_ rhs) ops)]
    dump_ (AST.Expr'Call _ callee args) = dump_struct "Expr'Call" [("callee", dump_ callee), ("args", dump_list dump_ args)]
    dump_ (AST.Expr'If _ _ cond true false) = dump_struct "Expr'If" [("cond", dump_ cond), ("true", dump_ true), ("false", dump_ false)]
    dump_ (AST.Expr'Case _ _ e arms) = dump_struct "Expr'Case" [("e", dump_ e), ("arms", dump_list (\ (pat, expr) -> dump_ pat >> DumpUtils.dump " -> " >> dump_ expr) arms)]
    dump_ (AST.Expr'TypeAnnotation _ ty e) = dump_struct "Expr'TypeAnnotation" [("ty", dump_ ty), ("e", dump_ e)]

instance Dumpable AST.Pattern where
    dump_ (AST.Pattern'Identifier i) = dump_struct "Pattern'Identifier" [("i", dump_ i)]
    dump_ (AST.Pattern'Wildcard _) = dump_struct "Pattern'Wildcard" []
    dump_ (AST.Pattern'Tuple _ items) = dump_struct "Pattern'Tuple" [("items", dump_list dump_ items)]
    dump_ (AST.Pattern'Named _ name _ subpat) = dump_struct "Pattern'Named" [("name", dump_ name), ("subpat", dump_ subpat)]

instance Dumpable AST.Identifier where
    dump_ (Located _ items) = DumpUtils.dump $ Text.intercalate "::" (map unlocate items)

dump_struct :: Text -> [(Text, DumpUtils.Dumper ())] -> DumpUtils.Dumper ()
dump_struct name fields =
    case map dump_field fields of
        [] -> DumpUtils.dump name >> DumpUtils.dump " {}"
        [field]
            | not $ DumpUtils.is_multiline field -> DumpUtils.dump name >> DumpUtils.dump " { " >> field >> DumpUtils.dump " }"

        fields -> DumpUtils.dump name >> DumpUtils.dump " {\n" >> DumpUtils.indent >> mapM (>> DumpUtils.dump ",\n") fields >> DumpUtils.dedent >> DumpUtils.dump "}"
    where
        dump_field (name, value) = DumpUtils.dump name >> DumpUtils.dump " = " >> value

dump_list :: (d -> DumpUtils.Dumper ()) -> [d] -> DumpUtils.Dumper ()
dump_list dump items =
    let dumped = map dump items
        any_multiline = any DumpUtils.is_multiline dumped
    in if null dumped
        then DumpUtils.dump "[]"
        else if any_multiline
            then DumpUtils.dump "[\n" >> DumpUtils.indent >> sequence (map (>> DumpUtils.dump ",\n") dumped) >> DumpUtils.dedent >> DumpUtils.dump "]" -- true if first
            else DumpUtils.dump "[" >> intercalate_commas True dumped >> DumpUtils.dump "]" -- true if first
    where
        intercalate_commas _ [] = pure ()
        intercalate_commas True (x:more) = x >> intercalate_commas False more
        intercalate_commas False (x:more) = DumpUtils.dump ", " >> x >> intercalate_commas False more
