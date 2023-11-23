module UHF.Phases.EvalTypeExprs
    ( eval
    ) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.Compiler as Compiler

import qualified UHF.IO.Located as Located
import UHF.IO.Span (Span)
import UHF.IO.Located (Located (Located, unlocate))
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Diagnostic.Codes

import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type

import qualified Data.Map as Map
import qualified Data.List as List

type DeclMap = Map.Map Text SIR.DeclKey
data DeclMapStack = DeclMapStack DeclMap (Maybe DeclMapStack)

type TypeVarArena = Arena.Arena Type.Var Type.TypeVarKey
type BoundValueArena = Arena.Arena (SIR.BoundValue ()) SIR.BoundValueKey
type ModuleDeclMap = Arena.Arena DeclMap SIR.ModuleKey
type DeclArena = Arena.Arena SIR.Decl SIR.DeclKey

type UnevaledDIden = [Located Text]
type UnevaledVIden = [Located Text] -- TODO: make ToSIR make this (Maybe [Located Text], Located Text)
type UnevaledPIden = [Located Text] -- TODO: make ToSIR make this (Maybe [Located Text], Located Text)

type UnevaledSIR = SIR.SIR UnevaledDIden UnevaledVIden UnevaledPIden () ()
type UnevaledModule = SIR.Module UnevaledDIden UnevaledVIden UnevaledPIden () ()
type UnevaledADT = Type.ADT UnevaledTypeExpr
type UnevaledTypeSynonym = Type.TypeSynonym UnevaledTypeExpr
type UnevaledTypeExpr = SIR.TypeExpr UnevaledDIden ()
type UnevaledBinding = SIR.Binding UnevaledDIden UnevaledVIden UnevaledPIden () ()
type UnevaledExpr = SIR.Expr UnevaledDIden UnevaledVIden UnevaledPIden () ()
type UnevaledPattern = SIR.Pattern UnevaledPIden ()

type UnevaledModuleArena = Arena.Arena UnevaledModule SIR.ModuleKey
type UnevaledADTArena = Arena.Arena UnevaledADT Type.ADTKey
type UnevaledTypeSynonymArena = Arena.Arena UnevaledTypeSynonym Type.TypeSynonymKey

type EvaledDIden = Maybe SIR.DeclKey
type EvaledVIden = (Maybe (Either () SIR.DeclKey), Located Text)
type EvaledPIden = (Maybe (Either () SIR.DeclKey), Located Text)

type EvaledSIR = SIR.SIR EvaledDIden EvaledVIden EvaledPIden () ()
type EvaledModule = SIR.Module EvaledDIden EvaledVIden EvaledPIden () ()
type EvaledADT = Type.ADT EvaledTypeExpr
type EvaledTypeSynonym = Type.TypeSynonym EvaledTypeExpr
type EvaledTypeExpr = SIR.TypeExpr EvaledDIden ()
type EvaledBinding = SIR.Binding EvaledDIden EvaledVIden EvaledPIden () ()
type EvaledExpr = SIR.Expr EvaledDIden EvaledVIden EvaledPIden () ()
type EvaledPattern = SIR.Pattern EvaledPIden ()

type EvaledModuleArena = Arena.Arena EvaledModule SIR.ModuleKey
type EvaledADTArena = Arena.Arena EvaledADT Type.ADTKey
type EvaledTypeSynonymArena = Arena.Arena EvaledTypeSynonym Type.TypeSynonymKey

data Error

instance Diagnostic.ToError Error where

eval :: UnevaledSIR -> Compiler.WithDiagnostics Void Void EvaledSIR
eval = todo
