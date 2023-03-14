{-# LANGUAGE FlexibleInstances #-}

module UHF.Data.IR.HIR.Dump (dump) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.DumpUtils as DumpUtils

import qualified UHF.Data.IR.HIR as HIR
import qualified UHF.Data.IR.Keys as Keys
import qualified UHF.Data.IR.Type as Type

import UHF.IO.Located (Located (unlocate))

import qualified Data.Text as Text

-- TODO: separate dump as definition and dump as reference

type Dumper iden type_expr type_info binary_ops_allowed = ReaderT (HIR.HIR iden type_expr type_info binary_ops_allowed) DumpUtils.Dumper

dump :: (DumpableType type_expr, DumpableIdentifier iden) => HIR.HIR iden type_expr type_info binary_ops_allowed -> Text
dump ir@(HIR.HIR decls _ _ _ mod) = DumpUtils.exec_dumper $ runReaderT (dump_decl $ Arena.get decls mod) ir

dump_text :: Text -> Dumper iden type_expr type_info binary_ops_allowed ()
dump_text = lift . DumpUtils.dump

get_bv :: Keys.BoundValueKey -> Dumper iden type_expr type_info binary_ops_allowed (HIR.BoundValue type_info)
get_bv k = reader (\ (HIR.HIR _ _ _ bvs _) -> Arena.get bvs k)
get_adt :: Keys.ADTKey -> Dumper iden type_expr type_info binary_ops_allowed (Type.ADT type_expr)
get_adt k = reader (\ (HIR.HIR _ adts _ _ _) -> Arena.get adts k)
get_type_syn :: Keys.TypeSynonymKey -> Dumper iden type_expr type_info binary_ops_allowed (Type.TypeSynonym type_expr)
get_type_syn k = reader (\ (HIR.HIR _ _ syns _ _) -> Arena.get syns k)

dump_decl :: (DumpableType type_expr, DumpableIdentifier iden) => HIR.Decl iden type_expr type_info binary_ops_allowed -> Dumper iden type_expr type_info binary_ops_allowed ()
dump_decl (HIR.Decl'Module _ bindings adts type_synonyms) = mapM_ (\ k -> get_adt k >>= dump_adt) adts >> mapM_ (\ k -> get_type_syn k >>= dump_type_synonym) type_synonyms >> mapM_ dump_binding bindings
dump_decl (HIR.Decl'Type _) = pure ()

dump_adt :: Type.ADT type_expr -> Dumper iden type_expr type_info binary_ops_allowed ()
dump_adt (Type.ADT name _) = dump_text "data " >> dump_text name >> dump_text ";\n" -- TODO

dump_type_synonym :: DumpableType type_expr => Type.TypeSynonym type_expr -> Dumper iden type_expr type_info binary_ops_allowed ()
dump_type_synonym (Type.TypeSynonym name expansion) = dump_text "typesyn " >> dump_text name >> dump_text " = " >> dump_type expansion >> dump_text ";\n"

dump_binding :: (DumpableType type_expr, DumpableIdentifier iden) => HIR.Binding iden type_expr type_info binary_ops_allowed -> Dumper iden type_expr type_info binary_ops_allowed ()
dump_binding (HIR.Binding pat _ init) =
    let init' = dump_expr init
    in ask >>= \ ir -> if DumpUtils.is_multiline (runReaderT init' ir)
        then dump_pattern pat >> dump_text " = \n" >> lift DumpUtils.indent >> init' >> dump_text "\n" >> lift DumpUtils.dedent >> dump_text ";\n"
        else dump_pattern pat >> dump_text " = " >> init' >> dump_text ";\n"

class DumpableIdentifier i where
    dump_iden :: i -> Dumper iden type_expr type_info binary_ops_allowed ()

instance DumpableIdentifier (HIR.NameContext, [Located Text]) where
    dump_iden (_, segments) = dump_text $ Text.intercalate "::" (map unlocate segments)
instance DumpableIdentifier (Located (Maybe Keys.BoundValueKey)) where
    dump_iden k = case unlocate k of
        Just k -> dump_iden k
        Nothing -> dump_text "<name resolution error>"
instance DumpableIdentifier Keys.BoundValueKey where
    dump_iden k = get_bv k >>= \ (HIR.BoundValue _ _) -> dump_text "_" >> dump_text (show $ Arena.unmake_key k) -- TODO: show names and dont use unmake_key
instance DumpableIdentifier (Maybe Keys.DeclKey) where
    dump_iden (Just k) = dump_text "decl_" >> dump_text (show $ Arena.unmake_key k)
    dump_iden Nothing = dump_text "<name resolution error>"

class DumpableType t where
    dump_type :: t -> Dumper iden type_expr type_info binary_ops_allowed ()
instance DumpableIdentifier iden => DumpableType (HIR.TypeExpr iden) where
    dump_type (HIR.TypeExpr'Identifier _ iden) = dump_iden iden
    dump_type (HIR.TypeExpr'Tuple a b) = dump_text "(" >> dump_type a >> dump_text ", " >> dump_type b >> dump_text ")"
    dump_type (HIR.TypeExpr'Poison _) = dump_text "poison"
instance DumpableType (Maybe (Type.Type Void)) where
    dump_type (Just ty) = dump_type ty
    dump_type Nothing = dump_text "<type error>"
instance DumpableType (Type.Type Void) where
    dump_type (Type.Type'ADT k) = get_adt k >>= \ (Type.ADT name _) -> dump_text name -- TODO: dump path
    dump_type (Type.Type'Synonym k) = get_type_syn k >>= \ (Type.TypeSynonym name _) -> dump_text name
    dump_type Type.Type'Int = dump_text "int"
    dump_type Type.Type'Float = dump_text "float"
    dump_type Type.Type'Char = dump_text "char"
    dump_type Type.Type'String = dump_text "string"
    dump_type Type.Type'Bool = dump_text "bool"
    dump_type (Type.Type'Function a r) = dump_type a >> dump_text " -> " >> dump_type r
    dump_type (Type.Type'Tuple a b) = dump_text "(" >> dump_type a >> dump_text ", " >> dump_type b >> dump_text ")"
    dump_type (Type.Type'Variable void) = absurd void

-- TODO: dump types too

-- TODO: deal with precedence

dump_expr :: (DumpableIdentifier iden, DumpableType type_expr) => HIR.Expr iden type_expr type_info binary_ops_allowed -> Dumper iden type_expr type_info binary_ops_allowed ()
dump_expr (HIR.Expr'Identifier _ _ i) = dump_iden i
dump_expr (HIR.Expr'Char _ _ c) = dump_text $ show c
dump_expr (HIR.Expr'String _ _ s) = dump_text $ show s
dump_expr (HIR.Expr'Int _ _ i) = dump_text $ show i
dump_expr (HIR.Expr'Float _ _ f) = dump_text $ show f
dump_expr (HIR.Expr'Bool _ _ b) = dump_text $ if b then "true" else "false"
dump_expr (HIR.Expr'Tuple _ _ a b) = dump_text "(" >> dump_expr a >> dump_text ", " >> dump_expr b >> dump_text ")"
dump_expr (HIR.Expr'Lambda _ _ param body) = dump_text "\\ " >> dump_pattern param >> dump_text " -> " >> dump_expr body -- TODO: decide if this should be \ (x) -> or \ x ->
dump_expr (HIR.Expr'Let _ _ bindings body) = dump_text "let {\n" >> lift DumpUtils.indent >> mapM_ dump_binding bindings >> lift DumpUtils.dedent >> dump_text "}\n" >> dump_expr body
dump_expr (HIR.Expr'BinaryOps _ _ _ first _) = dump_text "(" >> dump_expr first >> todo >> dump_text ")" -- TODO
dump_expr (HIR.Expr'Call _ _ callee arg) = dump_text "(" >> dump_expr callee >> dump_text "(" >> dump_expr arg >> dump_text "))"
dump_expr (HIR.Expr'If _ _ _ cond t f) = dump_text "if " >> dump_expr cond >> dump_text " then " >> dump_expr t >> dump_text " else " >> dump_expr f
dump_expr (HIR.Expr'Case _ _ _ _ _) = todo
dump_expr (HIR.Expr'TypeAnnotation _ _ ty e) = dump_text ":" >> dump_type ty >> dump_text ": " >> dump_expr e
dump_expr (HIR.Expr'Poison _ _) = dump_text "poison"

dump_pattern :: HIR.Pattern iden type_info -> Dumper iden type_expr type_info binary_ops_allowed ()
dump_pattern (HIR.Pattern'Identifier _ _ bnk) = dump_iden bnk
dump_pattern (HIR.Pattern'Wildcard _ _) = dump_text "_"
dump_pattern (HIR.Pattern'Tuple _ _ a b) = dump_text "(" >> dump_pattern a >> dump_text ", " >> dump_pattern b >> dump_text ")"
dump_pattern (HIR.Pattern'Named _ _ _ bnk subpat) = dump_text "@" >> dump_iden (unlocate bnk) >> dump_text " " >> dump_pattern subpat
dump_pattern (HIR.Pattern'Poison _ _) = dump_text "poison"
