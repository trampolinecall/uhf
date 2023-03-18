{-# LANGUAGE FlexibleInstances #-}

module UHF.Data.IR.ANFIR.Dump (dump) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.DumpUtils as DumpUtils

import qualified UHF.Data.IR.ANFIR as ANFIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.Dump as Type.Dump
import qualified UHF.Data.IR.ID as ID

-- TODO: dump types too

type Dumper ty poison_allowed = ReaderT (ANFIR.ANFIR ty poison_allowed) DumpUtils.Dumper

get_adt_arena :: Dumper ty poison_allowed (Arena.Arena (Type.ADT ty) Type.ADTKey)
get_adt_arena = reader (\ (ANFIR.ANFIR _ adts _ _ _ _) -> adts)
get_type_synonym_arena :: Dumper ty poison_allowed (Arena.Arena (Type.TypeSynonym ty) Type.TypeSynonymKey)
get_type_synonym_arena = reader (\ (ANFIR.ANFIR _ _ syns _ _ _) -> syns)

get_binding :: ANFIR.BindingKey -> Dumper ty poison_allowed (ANFIR.Binding ty poison_allowed)
get_binding k = reader (\ (ANFIR.ANFIR _ _ _ bindings _ _) -> Arena.get bindings k)
get_param :: ANFIR.ParamKey -> Dumper ty poison_allowed (ANFIR.Param ty)
get_param k = reader (\ (ANFIR.ANFIR _ _ _ _ params _) -> Arena.get params k)
get_adt :: Type.ADTKey -> Dumper ty poison_allowed (Type.ADT ty)
get_adt k = reader (\ (ANFIR.ANFIR _ adts _ _ _ _) -> Arena.get adts k)
get_type_synonym :: Type.TypeSynonymKey -> Dumper ty poison_allowed (Type.TypeSynonym ty)
get_type_synonym k = reader (\ (ANFIR.ANFIR _ _ type_synonyms _ _ _) -> Arena.get type_synonyms k)

dump :: DumpableType ty => ANFIR.ANFIR ty poison_allowed -> Text
dump ir@(ANFIR.ANFIR decls _ _ _ _ mod) = DumpUtils.exec_dumper $ runReaderT (define_decl $ Arena.get decls mod) ir

text :: Text -> Dumper ty poison_allowed ()
text = lift . DumpUtils.dump

define_decl :: DumpableType ty => ANFIR.Decl -> Dumper ty poison_allowed ()
define_decl (ANFIR.Decl'Module bindings adts type_synonyms) =
    ask >>= \ anfir ->
    get_adt_arena >>= \ adt_arena ->
    get_type_synonym_arena >>= \ type_synonym_arena ->
    mapM_ (lift . Type.Dump.define_adt adt_arena) adts >> mapM_ (lift . Type.Dump.define_type_synonym (\ ty -> runReaderT (refer_type ty) anfir) type_synonym_arena) type_synonyms >> mapM_ (\ k -> get_binding k >>= define_binding k) bindings
define_decl (ANFIR.Decl'Type _) = pure ()

refer_param :: ANFIR.ParamKey -> Dumper ty poison_allowed ()
refer_param key = get_param key >>= \ (ANFIR.Param id _) -> text (ID.stringify id)

refer_binding :: ANFIR.BindingKey -> Dumper ty poison_allowed ()
refer_binding key = ANFIR.binding_id <$> get_binding key >>= \ id -> text (ID.stringify id)

define_binding :: ANFIR.BindingKey -> ANFIR.Binding ty poison_allowed -> Dumper ty poison_allowed ()
define_binding key (ANFIR.Binding e) =
    let e' = expr e
    in ask >>= \ ir -> if DumpUtils.is_multiline (runReaderT e' ir)
        then refer_binding key >> text " =\n" >> lift DumpUtils.indent >> e' >> text "\n" >> lift DumpUtils.dedent >> text ";\n"
        else refer_binding key >> text " = " >> e' >> text ";\n"

class DumpableType t where
    refer_type :: t -> Dumper ty poison_allowed ()

instance DumpableType (Maybe (Type.Type Void)) where -- TODO: remove this
    refer_type (Just ty) = refer_type ty
    refer_type Nothing = text "<type error>"

instance DumpableType (Type.Type Void) where
    refer_type ty =
        get_adt_arena >>= \ adt_arena ->
        get_type_synonym_arena >>= \ type_synonym_arena ->
        lift (Type.Dump.refer_type adt_arena type_synonym_arena ty)

expr :: ANFIR.Expr ty poison_allowed -> Dumper ty poison_allowed ()
expr (ANFIR.Expr'Identifier _ _ bk) = refer_binding bk
expr (ANFIR.Expr'Int _ _ i) = text $ show i
expr (ANFIR.Expr'Float _ _ (n :% d)) = text $ "(" <> show n <> "/" <> show d <> ")"
expr (ANFIR.Expr'Bool _ _ b) = text $ if b then "true" else "false"
expr (ANFIR.Expr'Char _ _ c) = text $ show c
expr (ANFIR.Expr'String _ _ s) = text $ show s
expr (ANFIR.Expr'Tuple _ _ a b) = text "(" >> refer_binding a >> text ", " >> refer_binding b >> text ")"
expr (ANFIR.Expr'Lambda _ _ _ param bindings body) = text "\\ " >> refer_param param >> text " -> {\n" >> lift DumpUtils.indent >> mapM_ (\ k -> get_binding k >>= define_binding k) bindings >> lift DumpUtils.dedent >> text "}\n" >> refer_binding body -- TODO: dump captures
expr (ANFIR.Expr'Param _ _ pk) = refer_param pk
expr (ANFIR.Expr'Call _ _ callee arg) = refer_binding callee >> text "(" >> refer_binding arg >> text ")"
expr (ANFIR.Expr'Switch _ _ e arms) = text "switch " >> refer_binding e >> text " {\n" >> lift DumpUtils.indent >> mapM_ arm arms >> lift DumpUtils.dedent >> text "}"
    where
        arm (ANFIR.Switch'BoolLiteral b, expr) = (if b then text "true" else text "false") >> text " -> " >> refer_binding expr >> text "\n"
        arm (ANFIR.Switch'Tuple, expr) = text "(,) -> " >> refer_binding expr >> text "\n"
        arm (ANFIR.Switch'Default, expr) = text "-> " >> refer_binding expr >> text "\n"
expr (ANFIR.Expr'Seq _ _ a b) = text "seq " >> refer_binding a >> text ", " >> refer_binding b
expr (ANFIR.Expr'TupleDestructure1 _ _ other) = refer_binding other >> text ".0"
expr (ANFIR.Expr'TupleDestructure2 _ _ other) = refer_binding other >> text ".1"
expr (ANFIR.Expr'Poison _ _ _) = text "poison"
