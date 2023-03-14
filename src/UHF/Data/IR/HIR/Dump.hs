{-# LANGUAGE FlexibleInstances #-}

module UHF.Data.IR.HIR.Dump (dump) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.DumpUtils as DumpUtils

import qualified UHF.Data.IR.HIR as HIR
import qualified UHF.Data.IR.Keys as Keys
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.Dump as Type.Dump
import qualified UHF.Data.IR.ID as ID

import UHF.IO.Located (Located (unlocate))

import qualified Data.Text as Text

type Dumper iden type_expr type_info binary_ops_allowed = ReaderT (HIR.HIR iden type_expr type_info binary_ops_allowed) DumpUtils.Dumper

dump :: (DumpableType type_expr, DumpableIdentifier iden) => HIR.HIR iden type_expr type_info binary_ops_allowed -> Text
dump ir@(HIR.HIR decls _ _ _ mod) = DumpUtils.exec_dumper $ runReaderT (define_decl $ Arena.get decls mod) ir

text :: Text -> Dumper iden type_expr type_info binary_ops_allowed ()
text = lift . DumpUtils.dump

get_adt_arena :: Dumper iden type_expr type_info binary_ops_allowed (Arena.Arena (Type.ADT type_expr) Type.ADTKey)
get_adt_arena = reader (\ (HIR.HIR _ adts _ _ _) -> adts)
get_type_synonym_arena :: Dumper iden type_expr type_info binary_ops_allowed (Arena.Arena (Type.TypeSynonym type_expr) Type.TypeSynonymKey)
get_type_synonym_arena = reader (\ (HIR.HIR _ _ syns _ _) -> syns)

get_bv :: Keys.BoundValueKey -> Dumper iden type_expr type_info binary_ops_allowed (HIR.BoundValue type_info)
get_bv k = reader (\ (HIR.HIR _ _ _ bvs _) -> Arena.get bvs k)
get_decl :: Keys.DeclKey -> Dumper iden type_expr type_info binary_ops_allowed (HIR.Decl iden type_expr type_info binary_ops_allowed)
get_decl k = reader (\ (HIR.HIR decls _ _ _ _) -> Arena.get decls k)
get_adt :: Keys.ADTKey -> Dumper iden type_expr type_info binary_ops_allowed (Type.ADT type_expr)
get_adt k = reader (\ (HIR.HIR _ adts _ _ _) -> Arena.get adts k)
get_type_syn :: Keys.TypeSynonymKey -> Dumper iden type_expr type_info binary_ops_allowed (Type.TypeSynonym type_expr)
get_type_syn k = reader (\ (HIR.HIR _ _ syns _ _) -> Arena.get syns k)

define_decl :: (DumpableType type_expr, DumpableIdentifier iden) => HIR.Decl iden type_expr type_info binary_ops_allowed -> Dumper iden type_expr type_info binary_ops_allowed ()
define_decl (HIR.Decl'Module _ _ bindings adts type_synonyms) =
    ask >>= \ hir ->
    get_adt_arena >>= \ adt_arena ->
    get_type_synonym_arena >>= \ type_synonym_arena ->
    lift (mapM_ (Type.Dump.define_adt adt_arena) adts) >>
    lift (mapM_ (Type.Dump.define_type_synonym ((\ ty -> runReaderT (refer_type ty) hir)) type_synonym_arena) type_synonyms) >>
    mapM_ define_binding bindings
define_decl (HIR.Decl'Type _) = pure ()

define_binding :: (DumpableType type_expr, DumpableIdentifier iden) => HIR.Binding iden type_expr type_info binary_ops_allowed -> Dumper iden type_expr type_info binary_ops_allowed ()
define_binding (HIR.Binding pat _ init) =
    let init' = expr init
    in ask >>= \ ir -> if DumpUtils.is_multiline (runReaderT init' ir)
        then pattern pat >> text " = \n" >> lift DumpUtils.indent >> init' >> text "\n" >> lift DumpUtils.dedent >> text ";\n"
        else pattern pat >> text " = " >> init' >> text ";\n"

class DumpableIdentifier i where
    refer_iden :: i -> Dumper iden type_expr type_info binary_ops_allowed ()

refer_bv :: HIR.BoundValueKey -> Dumper iden type_expr type_info binary_ops_allowed ()
refer_bv k = get_bv k >>= \ (HIR.BoundValue id _ _) -> text (ID.stringify id)

refer_decl :: HIR.DeclKey -> Dumper iden type_expr type_info binary_ops_allowed ()
refer_decl k = get_decl k >>= \case
    HIR.Decl'Module id _ _ _ _ -> text $ ID.stringify id
    HIR.Decl'Type ty -> refer_type ty

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
    refer_type ty = get_adt_arena >>= \ adt_arena -> get_type_synonym_arena >>= \ type_synonym_arena -> lift (Type.Dump.refer_type adt_arena type_synonym_arena ty)

-- TODO: dump types too

-- TODO: deal with precedence

expr :: (DumpableIdentifier iden, DumpableType type_expr) => HIR.Expr iden type_expr type_info binary_ops_allowed -> Dumper iden type_expr type_info binary_ops_allowed ()
expr (HIR.Expr'Identifier _ _ i) = refer_iden i
expr (HIR.Expr'Char _ _ c) = text $ show c
expr (HIR.Expr'String _ _ s) = text $ show s
expr (HIR.Expr'Int _ _ i) = text $ show i
expr (HIR.Expr'Float _ _ (n :% d)) = text $ "(" <> show n <> "/" <> show d <> ")"
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
