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

-- TODO: dont dump decls, just dump module
-- TODO: remove Dumable class

type Dumper iden type_expr type_info binary_ops_allowed = ReaderT (HIR.HIR iden type_expr type_info binary_ops_allowed) DumpUtils.Dumper

dump :: (DumpableIdentifier iden, DumpableType type_expr) => HIR.HIR iden type_expr type_info binary_ops_allowed -> Text
dump ir@(HIR.HIR decls _ _ _) = DumpUtils.exec_dumper $ runReaderT (Arena.transformM dump_ decls) ir

class Dumpable d where
    dump_ :: DumpableType type_expr => d -> Dumper iden type_expr type_info binary_ops_allowed ()

instance Dumpable Text where
    dump_ = lift . DumpUtils.dump

dump_text :: DumpableType type_expr => Text -> Dumper iden type_expr type_info binary_ops_allowed ()
dump_text = dump_

get_bv :: Keys.BoundValueKey -> Dumper iden type_expr type_info binary_ops_allowed (HIR.BoundValue type_info)
get_bv k = reader (\ (HIR.HIR _ _ _ bvs) -> Arena.get bvs k)
get_adt :: Keys.ADTKey -> Dumper iden type_expr type_info binary_ops_allowed (Type.ADT type_expr)
get_adt k = reader (\ (HIR.HIR _ adts _ _) -> Arena.get adts k)
get_type_syn :: Keys.TypeSynonymKey -> Dumper iden type_expr type_info binary_ops_allowed (Type.TypeSynonym type_expr)
get_type_syn k = reader (\ (HIR.HIR _ _ syns _) -> Arena.get syns k)

instance (DumpableIdentifier iden, DumpableType type_expr) => Dumpable (HIR.Decl iden type_expr type_info binary_ops_allowed) where
    dump_ (HIR.Decl'Module _ bindings adts type_synonyms) = mapM_ (\ k -> get_adt k >>= dump_) adts >> mapM_ (\ k -> get_type_syn k >>= dump_) type_synonyms >> mapM_ dump_ bindings
    dump_ (HIR.Decl'Type ty) = pure ()

instance (DumpableType ty) => Dumpable (Type.ADT ty) where
    dump_ (Type.ADT name variants) = dump_text "data " >> dump_text name >> dump_text ";\n" -- TODO
instance (DumpableType ty) => Dumpable (Type.TypeSynonym ty) where
    dump_ (Type.TypeSynonym name expansion) = dump_text "typesyn " >> dump_text name >> dump_text " = " >> dump_type expansion >> dump_text ";\n"
instance (DumpableIdentifier iden, DumpableType type_expr) => Dumpable (HIR.Binding iden type_expr type_info binary_ops_allowed) where
    dump_ (HIR.Binding pat _ init) = dump_ pat >> dump_text " = " >> dump_ init >> dump_text ";\n"

class DumpableIdentifier i where
    dump_iden :: DumpableType type_expr => i -> Dumper iden type_expr type_info binary_ops_allowed ()

instance DumpableIdentifier (HIR.NameContext, [Located Text]) where
    dump_iden (_, segments) = dump_ $ Text.intercalate "::" (map unlocate segments)
instance DumpableIdentifier (Located (Maybe Keys.BoundValueKey)) where
    dump_iden k = case unlocate k of
        Just k -> dump_iden k
        Nothing -> dump_text "<name resolution error>"
instance DumpableIdentifier (Keys.BoundValueKey) where
    dump_iden k = get_bv k >>= \ (HIR.BoundValue _ ty) -> dump_text "_" >> dump_text (show $ Arena.unmake_key k) -- TODO: show names and dont use unmake_key
instance DumpableIdentifier (Maybe Keys.DeclKey) where
    dump_iden (Just k) = dump_text "decl_" >> dump_text (show $ Arena.unmake_key k)
    dump_iden Nothing = dump_text "<name resolution error>"

class DumpableType t where
    dump_type :: DumpableType type_expr => t -> Dumper iden type_expr type_info binary_ops_allowed ()
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
    dump_type (Type.Type'Int) = dump_text "int"
    dump_type (Type.Type'Float) = dump_text "float"
    dump_type (Type.Type'Char) = dump_text "char"
    dump_type (Type.Type'String) = dump_text "string"
    dump_type (Type.Type'Bool) = dump_text "bool"
    dump_type (Type.Type'Function a r) = dump_type a >> dump_text " -> " >> dump_type r
    dump_type (Type.Type'Tuple a b) = dump_text "(" >> dump_type a >> dump_text ", " >> dump_type b >> dump_text ")"
    dump_type (Type.Type'Variable void) = absurd void

-- TODO: dump types too

-- TODO: deal with precedence

instance (DumpableIdentifier iden, DumpableType type_expr) => Dumpable (HIR.Expr iden type_expr type_info binary_ops_allowed) where
    dump_ (HIR.Expr'Identifier typeinfo _ i) = dump_iden i
    dump_ (HIR.Expr'Char typeinfo _ c) = dump_text $ show c
    dump_ (HIR.Expr'String typeinfo _ s) = dump_text $ show s
    dump_ (HIR.Expr'Int typeinfo _ i) = dump_text $ show i
    dump_ (HIR.Expr'Float typeinfo _ f) = dump_text $ show f
    dump_ (HIR.Expr'Bool typeinfo _ b) = dump_text $ if b then "true" else "false"
    dump_ (HIR.Expr'Tuple typeinfo _ a b) = dump_text "(" >> dump_ a >> dump_text ", " >> dump_ b >> dump_text ")"
    dump_ (HIR.Expr'Lambda typeinfo _ param body) = dump_text "\\ " >> dump_ param >> dump_text " -> " >> dump_ body -- TODO: decide if this should be \ (x) -> or \ x ->
    dump_ (HIR.Expr'Let typeinfo _ bindings body) = dump_text "let {\n" >> lift DumpUtils.indent >> mapM_ dump_ bindings >> lift DumpUtils.dedent >> dump_text "}\n" >> dump_ body
    dump_ (HIR.Expr'BinaryOps _ typeinfo _ first ops) = dump_text "(" >> dump_ first >> todo >> dump_text ")" -- TODO
    dump_ (HIR.Expr'Call typeinfo _ callee arg) = dump_text "(" >> dump_ callee >> dump_text "(" >> dump_ arg >> dump_text "))"
    dump_ (HIR.Expr'If typeinfo _ _ cond t f) = dump_text "if " >> dump_ cond >> dump_text " then " >> dump_ t >> dump_text " else " >> dump_ f
    dump_ (HIR.Expr'Case typeinfo _ _ e arms) = todo
    dump_ (HIR.Expr'TypeAnnotation typeinfo _ ty e) = dump_text ":" >> dump_type ty >> dump_text ": " >> dump_ e
    dump_ (HIR.Expr'Poison typeinfo _) = dump_text "poison"

instance Dumpable (HIR.Pattern iden type_info) where
    dump_ (HIR.Pattern'Identifier typeinfo _ bnk) = dump_iden bnk
    dump_ (HIR.Pattern'Wildcard typeinfo _) = dump_text "_"
    dump_ (HIR.Pattern'Tuple typeinfo _ a b) = dump_text "(" >> dump_ a >> dump_text ", " >> dump_ b >> dump_text ")"
    dump_ (HIR.Pattern'Named typeinfo _ _ bnk subpat) = dump_text "@" >> dump_iden (unlocate bnk) >> dump_text " " >> dump_ subpat
    dump_ (HIR.Pattern'Poison typeinfo _) = dump_text "poison"
