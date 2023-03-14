{-# LANGUAGE FlexibleInstances #-}

module UHF.Data.IR.ANFIR.Dump (dump) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.DumpUtils as DumpUtils

import qualified UHF.Data.IR.ANFIR as ANFIR
import qualified UHF.Data.IR.Type as Type

-- TODO: dump types too

type Dumper ty poison_allowed = ReaderT (ANFIR.ANFIR ty poison_allowed) DumpUtils.Dumper

get_binding :: ANFIR.BindingKey -> Dumper ty poison_allowed (ANFIR.Binding ty poison_allowed)
get_binding k = reader (\ (ANFIR.ANFIR _ _ _ bindings _ _) -> Arena.get bindings k)
get_adt :: Type.ADTKey -> Dumper ty poison_allowed (Type.ADT ty)
get_adt k = reader (\ (ANFIR.ANFIR _ adts _ _ _ _) -> Arena.get adts k)
get_type_synonym :: Type.TypeSynonymKey -> Dumper ty poison_allowed (Type.TypeSynonym ty)
get_type_synonym k = reader (\ (ANFIR.ANFIR _ _ type_synonyms _ _ _) -> Arena.get type_synonyms k)

dump :: DumpableType ty => ANFIR.ANFIR ty poison_allowed -> Text
dump ir@(ANFIR.ANFIR decls _ _ _ _ mod) = DumpUtils.exec_dumper $ runReaderT (define_decl $ Arena.get decls mod) ir

text :: Text -> Dumper ty poison_allowed ()
text = lift . DumpUtils.dump

define_decl :: DumpableType ty => ANFIR.Decl -> Dumper ty poison_allowed ()
define_decl (ANFIR.Decl'Module bindings adts type_synonyms) = mapM_ define_adt adts >> mapM_ define_type_synonym type_synonyms >> mapM_ (\ k -> get_binding k >>= define_binding k) bindings
define_decl (ANFIR.Decl'Type _) = pure ()

refer_adt :: Type.ADTKey -> Dumper ty poison_allowed ()
refer_adt k = get_adt k >>= \ (Type.ADT name _) -> text name -- TODO: dump path
refer_type_synonym :: Type.TypeSynonymKey -> Dumper ty poison_allowed ()
refer_type_synonym k = get_type_synonym k >>= \ (Type.TypeSynonym name _) -> text name

define_adt :: Type.ADTKey -> Dumper ty poison_allowed ()
define_adt k = get_adt k >>= \ (Type.ADT name _) -> text "data " >> text name >> text ";\n" -- TODO
define_type_synonym :: DumpableType ty => Type.TypeSynonymKey -> Dumper ty poison_allowed ()
define_type_synonym k = get_type_synonym k >>= \ (Type.TypeSynonym name expansion) -> text "typesyn " >> text name >> text " = " >> refer_type expansion >> text ";\n"

refer_param :: ANFIR.ParamKey -> Dumper ty poison_allowed ()
refer_param key = text ("p_" <> show (Arena.unmake_key key)) -- TODO: dont use unmake_key

refer_binding :: ANFIR.BindingKey -> Dumper ty poison_allowed ()
refer_binding key = text ("_" <> show (Arena.unmake_key key)) -- TODO: dont use unmake_key

define_binding :: ANFIR.BindingKey -> ANFIR.Binding ty poison_allowed -> Dumper ty poison_allowed ()
define_binding key (ANFIR.Binding e) =
    let e' = expr e
    in ask >>= \ ir -> if DumpUtils.is_multiline (runReaderT e' ir)
        then refer_binding key >> text " = \n" >> lift DumpUtils.indent >> e' >> text "\n" >> lift DumpUtils.dedent >> text ";\n"
        else refer_binding key >> text " = " >> e' >> text ";\n"

-- TODO: put this in Type.Dump? because this is duplicated across all the IR dump modules
class DumpableType t where
    refer_type :: t -> Dumper ty poison_allowed ()

instance DumpableType (Maybe (Type.Type Void)) where -- TODO: remove this
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

expr :: ANFIR.Expr ty poison_allowed -> Dumper ty poison_allowed ()
expr (ANFIR.Expr'Identifier _ bk) = refer_binding bk
expr (ANFIR.Expr'Int _ i) = text $ show i
expr (ANFIR.Expr'Float _ f) = text $ show f
expr (ANFIR.Expr'Bool _ b) = text $ if b then "true" else "false"
expr (ANFIR.Expr'Char _ c) = text $ show c
expr (ANFIR.Expr'String _ s) = text $ show s
expr (ANFIR.Expr'Tuple _ a b) = text "(" >> refer_binding a >> text ", " >> refer_binding b >> text ")"
expr (ANFIR.Expr'Lambda _ _ param bindings body) = text "\\ " >> refer_param param >> text " -> {\n" >> lift DumpUtils.indent >> mapM_ (\ k -> get_binding k >>= define_binding k) bindings >> lift DumpUtils.dedent >> text "}\n" >> refer_binding body -- TODO: dump captures
expr (ANFIR.Expr'Param _ pk) = refer_param pk
expr (ANFIR.Expr'Call _ callee arg) = refer_binding callee >> text "(" >> refer_binding arg >> text ")"
expr (ANFIR.Expr'Switch _ e arms) = text "switch " >> refer_binding e >> text " {\n" >> lift DumpUtils.indent >> mapM_ arm arms >> lift DumpUtils.dedent >> text "}"
    where
        arm (ANFIR.Switch'BoolLiteral b, expr) = (if b then text "true" else text "false") >> text " -> " >> refer_binding expr >> text "\n"
        arm (ANFIR.Switch'Tuple, expr) = text "(,) -> " >> refer_binding expr >> text "\n"
        arm (ANFIR.Switch'Default, expr) = text "-> " >> refer_binding expr >> text "\n"
expr (ANFIR.Expr'Seq _ a b) = text "seq " >> refer_binding a >> text ", " >> refer_binding b
expr (ANFIR.Expr'TupleDestructure1 _ other) = refer_binding other >> text ".0"
expr (ANFIR.Expr'TupleDestructure2 _ other) = refer_binding other >> text ".1"
expr (ANFIR.Expr'Poison _ _) = text "poison"
