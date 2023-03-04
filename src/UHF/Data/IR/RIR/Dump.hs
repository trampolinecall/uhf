module UHF.Data.IR.RIR.Dump (dump) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.DumpUtils as DumpUtils

import qualified UHF.Data.IR.HIR as HIR
import qualified UHF.Data.IR.RIR as RIR
import qualified UHF.Data.IR.Keys as IR.Keys
import qualified UHF.Data.IR.Type as IR.Type

-- TODO: dont dump decls, just dump module

type IR = (Arena.Arena RIR.Decl IR.Keys.DeclKey, Arena.Arena (HIR.ADT (Maybe (IR.Type.Type Void))) IR.Keys.ADTKey, Arena.Arena (HIR.TypeSynonym (Maybe (IR.Type.Type Void))) IR.Keys.TypeSynonymKey, Arena.Arena (HIR.BoundValue (Maybe (IR.Type.Type Void))) IR.Keys.BoundValueKey)
type Dumper = ReaderT IR DumpUtils.Dumper

dump :: IR -> Text
dump ir@(decls, _, _, _) = DumpUtils.exec_dumper $ runReaderT (Arena.transformM dump_decl decls) ir

dump_decl :: RIR.Decl -> Dumper ()
dump_decl (HIR.Decl'Module _ bindings) = mapM_ dump_ bindings
dump_decl (HIR.Decl'Type ty) = pure ()
