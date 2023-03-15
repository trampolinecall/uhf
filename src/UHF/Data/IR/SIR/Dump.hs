{-# LANGUAGE FlexibleInstances #-}

module UHF.Data.IR.SIR.Dump (dump) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.DumpUtils as DumpUtils

import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Keys as Keys
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.Dump as Type.Dump
import qualified UHF.Data.IR.ID as ID

import UHF.IO.Located (Located (unlocate))

import qualified Data.Text as Text

type Dumper iden type_expr type_info binary_ops_allowed = ReaderT (SIR.SIR iden type_expr type_info binary_ops_allowed) DumpUtils.Dumper

dump :: (DumpableType type_expr, DumpableIdentifier iden) => SIR.SIR iden type_expr type_info binary_ops_allowed -> Text
dump ir@(SIR.SIR decls _ _ _ mod) = DumpUtils.exec_dumper $ runReaderT (define_decl $ Arena.get decls mod) ir

text :: Text -> Dumper iden type_expr type_info binary_ops_allowed ()
text = lift . DumpUtils.dump

get_adt_arena :: Dumper iden type_expr type_info binary_ops_allowed (Arena.Arena (Type.ADT type_expr) Type.ADTKey)
get_adt_arena = reader (\ (SIR.SIR _ adts _ _ _) -> adts)
get_type_synonym_arena :: Dumper iden type_expr type_info binary_ops_allowed (Arena.Arena (Type.TypeSynonym type_expr) Type.TypeSynonymKey)
get_type_synonym_arena = reader (\ (SIR.SIR _ _ syns _ _) -> syns)

get_bv :: Keys.BoundValueKey -> Dumper iden type_expr type_info binary_ops_allowed (SIR.BoundValue type_info)
get_bv k = reader (\ (SIR.SIR _ _ _ bvs _) -> Arena.get bvs k)
get_decl :: Keys.DeclKey -> Dumper iden type_expr type_info binary_ops_allowed (SIR.Decl iden type_expr type_info binary_ops_allowed)
get_decl k = reader (\ (SIR.SIR decls _ _ _ _) -> Arena.get decls k)
get_adt :: Keys.ADTKey -> Dumper iden type_expr type_info binary_ops_allowed (Type.ADT type_expr)
get_adt k = reader (\ (SIR.SIR _ adts _ _ _) -> Arena.get adts k)
get_type_syn :: Keys.TypeSynonymKey -> Dumper iden type_expr type_info binary_ops_allowed (Type.TypeSynonym type_expr)
get_type_syn k = reader (\ (SIR.SIR _ _ syns _ _) -> Arena.get syns k)

define_decl :: (DumpableType type_expr, DumpableIdentifier iden) => SIR.Decl iden type_expr type_info binary_ops_allowed -> Dumper iden type_expr type_info binary_ops_allowed ()
define_decl (SIR.Decl'Module _ _ bindings adts type_synonyms) =
    ask >>= \ sir ->
    get_adt_arena >>= \ adt_arena ->
    get_type_synonym_arena >>= \ type_synonym_arena ->
    lift (mapM_ (Type.Dump.define_adt adt_arena) adts) >>
    lift (mapM_ (Type.Dump.define_type_synonym ((\ ty -> runReaderT (refer_type ty) sir)) type_synonym_arena) type_synonyms) >>
    mapM_ define_binding bindings
define_decl (SIR.Decl'Type _) = pure ()

define_binding :: (DumpableType type_expr, DumpableIdentifier iden) => SIR.Binding iden type_expr type_info binary_ops_allowed -> Dumper iden type_expr type_info binary_ops_allowed ()
define_binding (SIR.Binding pat _ init) =
    let init' = expr init
    in ask >>= \ ir -> if DumpUtils.is_multiline (runReaderT init' ir)
        then pattern pat >> text " = \n" >> lift DumpUtils.indent >> init' >> text "\n" >> lift DumpUtils.dedent >> text ";\n"
        else pattern pat >> text " = " >> init' >> text ";\n"

class DumpableIdentifier i where
    refer_iden :: i -> Dumper iden type_expr type_info binary_ops_allowed ()

refer_bv :: SIR.BoundValueKey -> Dumper iden type_expr type_info binary_ops_allowed ()
refer_bv k = get_bv k >>= \ (SIR.BoundValue id _ _) -> text (ID.stringify id)

refer_decl :: SIR.DeclKey -> Dumper iden type_expr type_info binary_ops_allowed ()
refer_decl k = get_decl k >>= \case
    SIR.Decl'Module id _ _ _ _ -> text $ ID.stringify id
    SIR.Decl'Type ty -> refer_type ty

instance DumpableIdentifier (SIR.NameContext, [Located Text]) where
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
instance DumpableIdentifier iden => DumpableType (SIR.TypeExpr iden) where
    refer_type (SIR.TypeExpr'Identifier _ iden) = refer_iden iden
    refer_type (SIR.TypeExpr'Tuple a b) = text "(" >> refer_type a >> text ", " >> refer_type b >> text ")"
    refer_type (SIR.TypeExpr'Poison _) = text "poison"
instance DumpableType (Maybe (Type.Type Void)) where
    refer_type (Just ty) = refer_type ty
    refer_type Nothing = text "<type error>"
instance DumpableType (Type.Type Void) where
    refer_type ty = get_adt_arena >>= \ adt_arena -> get_type_synonym_arena >>= \ type_synonym_arena -> lift (Type.Dump.refer_type adt_arena type_synonym_arena ty)

-- TODO: dump types too

-- TODO: deal with precedence

expr :: (DumpableIdentifier iden, DumpableType type_expr) => SIR.Expr iden type_expr type_info binary_ops_allowed -> Dumper iden type_expr type_info binary_ops_allowed ()
expr (SIR.Expr'Identifier _ _ _ i) = refer_iden i
expr (SIR.Expr'Char _ _ _ c) = text $ show c
expr (SIR.Expr'String _ _ _ s) = text $ show s
expr (SIR.Expr'Int _ _ _ i) = text $ show i
expr (SIR.Expr'Float _ _ _ (n :% d)) = text $ "(" <> show n <> "/" <> show d <> ")"
expr (SIR.Expr'Bool _ _ _ b) = text $ if b then "true" else "false"
expr (SIR.Expr'Tuple _ _ _ a b) = text "(" >> expr a >> text ", " >> expr b >> text ")"
expr (SIR.Expr'Lambda _ _ _ param body) = text "\\ " >> pattern param >> text " -> " >> expr body -- TODO: decide if this should be \ (x) -> or \ x ->
expr (SIR.Expr'Let _ _ _ bindings body) = text "let {\n" >> lift DumpUtils.indent >> mapM_ define_binding bindings >> lift DumpUtils.dedent >> text "}\n" >> expr body
expr (SIR.Expr'BinaryOps _ _ _ _ first _) = text "(" >> expr first >> todo >> text ")" -- TODO
expr (SIR.Expr'Call _ _ _ callee arg) = text "(" >> expr callee >> text "(" >> expr arg >> text "))"
expr (SIR.Expr'If _ _ _ _ cond t f) = text "if " >> expr cond >> text " then " >> expr t >> text " else " >> expr f
expr (SIR.Expr'Case _ _ _ _ _ _) = todo
expr (SIR.Expr'TypeAnnotation _ _ _ ty e) = text ":" >> refer_type ty >> text ": " >> expr e
expr (SIR.Expr'Poison _ _ _) = text "poison"

pattern :: SIR.Pattern iden type_info -> Dumper iden type_expr type_info binary_ops_allowed ()
pattern (SIR.Pattern'Identifier _ _ bnk) = refer_iden bnk
pattern (SIR.Pattern'Wildcard _ _) = text "_"
pattern (SIR.Pattern'Tuple _ _ a b) = text "(" >> pattern a >> text ", " >> pattern b >> text ")"
pattern (SIR.Pattern'Named _ _ _ bnk subpat) = text "@" >> refer_iden (unlocate bnk) >> text " " >> pattern subpat
pattern (SIR.Pattern'Poison _ _) = text "poison"
