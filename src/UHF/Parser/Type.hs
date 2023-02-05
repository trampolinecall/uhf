module UHF.Parser.Type(type_) where

import UHF.Util.Prelude

import qualified UHF.Parser.PEG as Parser
import qualified UHF.Parser.Error as Error

import qualified UHF.IO.Location as Location
import qualified UHF.IO.File as File
import qualified UHF.IO.Location.SpanHelper as SpanHelper

import qualified UHF.Token as Token

import qualified UHF.AST as AST

import qualified Data.InfList as InfList

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

import qualified Control.Monad.Trans.State as State

type_ :: Parser.Parser AST.Type
type_ =
    Parser.consume "type" (Token.AlphaIdentifier ()) >>= \ (Location.Located iden_sp (Token.AlphaIdentifier iden)) ->
    pure (AST.Type'Identifier (Location.Located iden_sp iden))
