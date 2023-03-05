{-# LANGUAGE FlexibleInstances #-}

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
get_adt :: Keys.ADTKey -> Dumper ty poison_allowed (Type.ADT ty)
get_adt k = reader (\ (ANFIR.ANFIR _ adts _ _ _) -> Arena.get adts k)
get_type_synonym :: Keys.TypeSynonymKey -> Dumper ty poison_allowed (Type.TypeSynonym ty)
get_type_synonym k = reader (\ (ANFIR.ANFIR _ _ type_synonyms _ _) -> Arena.get type_synonyms k)

dump :: DumpableType ty => ANFIR.ANFIR ty poison_allowed -> Text
dump ir@(ANFIR.ANFIR decls _ _ _ _) = DumpUtils.exec_dumper $ runReaderT (Arena.transformM dump_decl decls) ir

dump_text :: Text -> Dumper ty poison_allowed ()
dump_text = lift . DumpUtils.dump

dump_decl :: DumpableType ty => ANFIR.Decl -> Dumper ty poison_allowed ()
dump_decl (ANFIR.Decl'Module bindings adts type_synonyms) = mapM_ (\ k -> get_adt k >>= dump_adt) adts >> mapM_ (\ k -> get_type_synonym k >>= dump_type_synonym) type_synonyms >> mapM_ (\ k -> get_binding k >>= dump_binding k) bindings
dump_decl (ANFIR.Decl'Type _) = pure ()

dump_adt :: Type.ADT ty -> Dumper ty poison_allowed ()
dump_adt (Type.ADT name _) = dump_text "data " >> dump_text name >> dump_text ";\n" -- TODO
dump_type_synonym :: DumpableType ty => Type.TypeSynonym ty -> Dumper ty poison_allowed ()
dump_type_synonym (Type.TypeSynonym name expansion) = dump_text "typesyn " >> dump_text name >> dump_text " = " >> dump_type expansion >> dump_text ";\n"

dump_binding_key :: ANFIR.BindingKey -> Dumper ty poison_allowed ()
dump_binding_key key = dump_text ("_" <> show (Arena.unmake_key key))
dump_param_key :: ANFIR.ParamKey -> Dumper ty poison_allowed ()
dump_param_key key = dump_text ("p_" <> show (Arena.unmake_key key))

dump_binding :: ANFIR.BindingKey -> ANFIR.Binding ty poison_allowed -> Dumper ty poison_allowed ()
dump_binding key (ANFIR.Binding expr) = dump_binding_key key >> dump_text " = " >> dump_expr expr >> dump_text ";\n" -- TODO: dont use unmake_key

-- TODO: put this in Type.Dump? because this is duplicated across all the IR dump modules
class DumpableType t where
    dump_type :: t -> Dumper ty poison_allowed ()

instance DumpableType (Maybe (Type.Type Void)) where -- TODO: remove this
    dump_type (Just ty) = dump_type ty
    dump_type Nothing = dump_text "<type error>"

instance DumpableType (Type.Type Void) where
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

dump_expr :: ANFIR.Expr ty poison_allowed -> Dumper ty poison_allowed ()
dump_expr (ANFIR.Expr'Identifier _ bk) = dump_binding_key bk
dump_expr (ANFIR.Expr'Int _ i) = dump_text $ show i
dump_expr (ANFIR.Expr'Float _ f) = dump_text $ show f
dump_expr (ANFIR.Expr'Bool _ b) = dump_text $ if b then "true" else "false"
dump_expr (ANFIR.Expr'Char _ c) = dump_text $ show c
dump_expr (ANFIR.Expr'String _ s) = dump_text $ show s
dump_expr (ANFIR.Expr'Tuple _ a b) = dump_text "(" >> dump_binding_key a >> dump_text ", " >> dump_binding_key b >> dump_text ")"
dump_expr (ANFIR.Expr'Lambda _ param bindings body) = dump_text "\\ " >> dump_param_key param >> dump_text " -> {\n" >> lift DumpUtils.indent >> mapM_ (\ k -> get_binding k >>= dump_binding k) bindings >> lift DumpUtils.dedent >> dump_text "}\n" >> dump_binding_key body
dump_expr (ANFIR.Expr'Param _ pk) = dump_param_key pk
dump_expr (ANFIR.Expr'Call _ callee arg) = dump_binding_key callee >> dump_text "(" >> dump_binding_key arg >> dump_text ")"
dump_expr (ANFIR.Expr'Switch _ e arms) = dump_text "switch " >> dump_binding_key e >> dump_text " {\n" >> lift DumpUtils.indent >> mapM_ dump_arm arms >> lift DumpUtils.dedent >> dump_text "}"
    where
        dump_arm (ANFIR.Switch'BoolLiteral b, expr) = (if b then dump_text "true" else dump_text "false") >> dump_text " -> " >> dump_binding_key expr >> dump_text "\n"
        dump_arm (ANFIR.Switch'Tuple, expr) = dump_text "(,) -> " >> dump_binding_key expr >> dump_text "\n"
        dump_arm (ANFIR.Switch'Default, expr) = dump_text "-> " >> dump_binding_key expr >> dump_text "\n"
dump_expr (ANFIR.Expr'TupleDestructure1 _ other) = dump_binding_key other >> dump_text ".0"
dump_expr (ANFIR.Expr'TupleDestructure2 _ other) = dump_binding_key other >> dump_text ".1"
dump_expr (ANFIR.Expr'Poison _ _) = dump_text "poison"
