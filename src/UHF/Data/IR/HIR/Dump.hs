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

type Dumper iden type_expr type_info binary_ops_allowed = ReaderT (HIR.HIR iden type_expr type_info binary_ops_allowed) DumpUtils.Dumper

dump :: (DumpableType type_expr, DumpableIdentifier iden) => HIR.HIR iden type_expr type_info binary_ops_allowed -> Text
dump ir@(HIR.HIR decls _ _ _ mod) = DumpUtils.exec_dumper $ runReaderT (define_decl $ Arena.get decls mod) ir

text :: Text -> Dumper iden type_expr type_info binary_ops_allowed ()
text = lift . DumpUtils.dump

get_bv :: Keys.BoundValueKey -> Dumper iden type_expr type_info binary_ops_allowed (HIR.BoundValue type_info)
get_bv k = reader (\ (HIR.HIR _ _ _ bvs _) -> Arena.get bvs k)
get_adt :: Keys.ADTKey -> Dumper iden type_expr type_info binary_ops_allowed (Type.ADT type_expr)
get_adt k = reader (\ (HIR.HIR _ adts _ _ _) -> Arena.get adts k)
get_type_syn :: Keys.TypeSynonymKey -> Dumper iden type_expr type_info binary_ops_allowed (Type.TypeSynonym type_expr)
get_type_syn k = reader (\ (HIR.HIR _ _ syns _ _) -> Arena.get syns k)

define_decl :: (DumpableType type_expr, DumpableIdentifier iden) => HIR.Decl iden type_expr type_info binary_ops_allowed -> Dumper iden type_expr type_info binary_ops_allowed ()
define_decl (HIR.Decl'Module _ bindings adts type_synonyms) = mapM_ define_adt adts >> mapM_ define_type_synonym type_synonyms >> mapM_ define_binding bindings
define_decl (HIR.Decl'Type _) = pure ()

define_adt :: Type.ADTKey -> Dumper iden type_expr type_info binary_ops_allowed ()
define_adt k = get_adt k >>= \ (Type.ADT name _) -> text "data " >> text name >> text ";\n" -- TODO

define_type_synonym :: DumpableType type_expr => Type.TypeSynonymKey -> Dumper iden type_expr type_info binary_ops_allowed ()
define_type_synonym k = get_type_syn k >>= \ (Type.TypeSynonym name expansion) -> text "typesyn " >> text name >> text " = " >> refer_type expansion >> text ";\n"

refer_adt :: Type.ADTKey -> Dumper iden type_expr type_info binary_ops_allowed ()
refer_adt k = get_adt k >>= \ (Type.ADT name _) -> text name -- TODO: dump path

refer_type_synonym :: Type.TypeSynonymKey -> Dumper iden type_expr type_info binary_ops_allowed ()
refer_type_synonym k = get_type_syn k >>= \ (Type.TypeSynonym name _) -> text name

define_binding :: (DumpableType type_expr, DumpableIdentifier iden) => HIR.Binding iden type_expr type_info binary_ops_allowed -> Dumper iden type_expr type_info binary_ops_allowed ()
define_binding (HIR.Binding pat _ init) =
    let init' = expr init
    in ask >>= \ ir -> if DumpUtils.is_multiline (runReaderT init' ir)
        then pattern pat >> text " = \n" >> lift DumpUtils.indent >> init' >> text "\n" >> lift DumpUtils.dedent >> text ";\n"
        else pattern pat >> text " = " >> init' >> text ";\n"

class DumpableIdentifier i where
    refer_iden :: i -> Dumper iden type_expr type_info binary_ops_allowed ()

refer_bv :: HIR.BoundValueKey -> Dumper iden type_expr type_info binary_ops_allowed ()
refer_bv k = get_bv k >>= \ (HIR.BoundValue _ _) -> text "_" >> text (show $ Arena.unmake_key k) -- TODO: show names and dont use unmake_key

refer_decl :: HIR.DeclKey -> Dumper iden type_expr type_info binary_ops_allowed ()
refer_decl k = text "decl_" >> text (show $ Arena.unmake_key k)

instance DumpableIdentifier (HIR.NameContext, [Located Text]) where
    refer_iden (_, segments) = text $ Text.intercalate "::" (map unlocate segments)
instance DumpableIdentifier (Located (Maybe Keys.BoundValueKey)) where
    refer_iden k = case unlocate k of
        Just k -> refer_iden k
        Nothing -> text "<name resolution error>"
instance DumpableIdentifier Keys.BoundValueKey where
    refer_iden k = refer_bv k
instance DumpableIdentifier (Maybe Keys.DeclKey) where
    refer_iden (Just k) = refer_decl k
    refer_iden Nothing = text "<name resolution error>"

class DumpableType t where
    refer_type :: t -> Dumper iden type_expr type_info binary_ops_allowed ()
instance DumpableIdentifier iden => DumpableType (HIR.TypeExpr iden) where
    refer_type (HIR.TypeExpr'Identifier _ iden) = refer_iden iden
    refer_type (HIR.TypeExpr'Tuple a b) = text "(" >> refer_type a >> text ", " >> refer_type b >> text ")"
    refer_type (HIR.TypeExpr'Poison _) = text "poison"
instance DumpableType (Maybe (Type.Type Void)) where
    refer_type (Just ty) = refer_type ty
    refer_type Nothing = text "<type error>"
instance DumpableType (Type.Type Void) where
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

-- TODO: dump types too

-- TODO: deal with precedence

expr :: (DumpableIdentifier iden, DumpableType type_expr) => HIR.Expr iden type_expr type_info binary_ops_allowed -> Dumper iden type_expr type_info binary_ops_allowed ()
expr (HIR.Expr'Identifier _ _ i) = refer_iden i
expr (HIR.Expr'Char _ _ c) = text $ show c
expr (HIR.Expr'String _ _ s) = text $ show s
expr (HIR.Expr'Int _ _ i) = text $ show i
expr (HIR.Expr'Float _ _ f) = text $ show f
expr (HIR.Expr'Bool _ _ b) = text $ if b then "true" else "false"
expr (HIR.Expr'Tuple _ _ a b) = text "(" >> expr a >> text ", " >> expr b >> text ")"
expr (HIR.Expr'Lambda _ _ param body) = text "\\ " >> pattern param >> text " -> " >> expr body -- TODO: decide if this should be \ (x) -> or \ x ->
expr (HIR.Expr'Let _ _ bindings body) = text "let {\n" >> lift DumpUtils.indent >> mapM_ define_binding bindings >> lift DumpUtils.dedent >> text "}\n" >> expr body
expr (HIR.Expr'BinaryOps _ _ _ first _) = text "(" >> expr first >> todo >> text ")" -- TODO
expr (HIR.Expr'Call _ _ callee arg) = text "(" >> expr callee >> text "(" >> expr arg >> text "))"
expr (HIR.Expr'If _ _ _ cond t f) = text "if " >> expr cond >> text " then " >> expr t >> text " else " >> expr f
expr (HIR.Expr'Case _ _ _ _ _) = todo
expr (HIR.Expr'TypeAnnotation _ _ ty e) = text ":" >> refer_type ty >> text ": " >> expr e
expr (HIR.Expr'Poison _ _) = text "poison"

pattern :: HIR.Pattern iden type_info -> Dumper iden type_expr type_info binary_ops_allowed ()
pattern (HIR.Pattern'Identifier _ _ bnk) = refer_iden bnk
pattern (HIR.Pattern'Wildcard _ _) = text "_"
pattern (HIR.Pattern'Tuple _ _ a b) = text "(" >> pattern a >> text ", " >> pattern b >> text ")"
pattern (HIR.Pattern'Named _ _ _ bnk subpat) = text "@" >> refer_iden (unlocate bnk) >> text " " >> pattern subpat
pattern (HIR.Pattern'Poison _ _) = text "poison"
