module UHF.Data.IR.ANFIR.Dump (dump) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.DumpUtils as DumpUtils

import qualified UHF.Data.IR.ANFIR as ANFIR
import qualified UHF.Data.IR.HIR as HIR
import qualified UHF.Data.IR.Keys as IR.Keys
import qualified UHF.Data.IR.Type as IR.Type

-- TODO: dont dump decls, just dump module

type IR = (Arena.Arena ANFIR.Decl IR.Keys.DeclKey, Arena.Arena (HIR.ADT (Maybe (IR.Type.Type Void))) IR.Keys.ADTKey, Arena.Arena (HIR.TypeSynonym (Maybe (IR.Type.Type Void))) IR.Keys.TypeSynonymKey, Arena.Arena (ANFIR.Binding (Maybe (IR.Type.Type Void)) ()) IR.Keys.BindingKey, Arena.Arena (ANFIR.Param (Maybe (IR.Type.Type Void))) IR.Keys.ParamKey)
type Dumper = ReaderT IR DumpUtils.Dumper

dump :: IR -> Text
dump ir@(decls, adts, type_synonyms, bindings, params) = DumpUtils.exec_dumper $ runReaderT (Arena.transformM dump_decl decls) ir

dump_decl :: ANFIR.Decl -> Dumper ()
dump_decl = todo
