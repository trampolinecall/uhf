module UHF.Type
    ( typecheck
    ) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.IR as IR

import UHF.IO.Location (Span)

import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Diagnostic.Codes
import qualified UHF.Diagnostic.Sections.Underlines as Underlines

type TypeExpr = IR.TypeExpr (Maybe IR.DeclKey)
newtype TypeVarKey = TypeVarKey Int
instance Arena.Key TypeVarKey where
    make_key = TypeVarKey
    unmake_key (TypeVarKey i) = i
data TypeVar
    = BoundValue IR.BoundNameKey
    | Expr Span
    | Pattern Span
    deriving Show

type TypeWithVars = IR.Type TypeVarKey
type Type = IR.Type Void

type UntypedNominalType = IR.NominalType TypeExpr
type UntypedBinding = IR.Binding (Maybe IR.BoundNameKey) TypeExpr ()
type UntypedExpr = IR.Expr (Maybe IR.BoundNameKey) TypeExpr ()
type UntypedPattern = IR.Pattern (Maybe IR.BoundNameKey) ()
type UntypedBoundName = IR.BoundName ()

type UntypedBindingArena = Arena.Arena UntypedBinding IR.BindingKey
type UntypedNominalTypeArena = Arena.Arena UntypedNominalType IR.NominalTypeKey
type UntypedBoundNameArena = Arena.Arena UntypedBoundName IR.BoundNameKey

type TypedWithVarsNominalType = IR.NominalType TypeWithVars
type TypedWithVarsBinding = IR.Binding (Maybe IR.BoundNameKey) TypeWithVars TypeWithVars
type TypedWithVarsExpr = IR.Expr (Maybe IR.BoundNameKey) TypeWithVars TypeWithVars
type TypedWithVarsPattern = IR.Pattern (Maybe IR.BoundNameKey) TypeWithVars
type TypedWithVarsBoundName = IR.BoundName TypeWithVars

type TypedWithVarsBindingArena = Arena.Arena TypedWithVarsBinding IR.BindingKey
type TypedWithVarsNominalTypeArena = Arena.Arena TypedWithVarsNominalType IR.NominalTypeKey
type TypedWithVarsBoundNameArena = Arena.Arena TypedWithVarsBoundName IR.BoundNameKey

type TypedNominalType = IR.NominalType Type
type TypedBinding = IR.Binding (Maybe IR.BoundNameKey) Type Type
type TypedExpr = IR.Expr (Maybe IR.BoundNameKey) Type Type
type TypedPattern = IR.Pattern (Maybe IR.BoundNameKey) Type
type TypedBoundName = IR.BoundName Type

type TypedBindingArena = Arena.Arena TypedBinding IR.BindingKey
type TypedNominalTypeArena = Arena.Arena TypedNominalType IR.NominalTypeKey
type TypedBoundNameArena = Arena.Arena TypedBoundName IR.BoundNameKey

type Decl = IR.Decl

type DeclArena = Arena.Arena Decl IR.DeclKey

data Error
instance Diagnostic.IsError Error where

typecheck :: (DeclArena, UntypedNominalTypeArena, UntypedBindingArena, UntypedBoundNameArena) -> Writer [Error] (DeclArena, TypedNominalTypeArena, TypedBindingArena, TypedBoundNameArena)
typecheck = todo

