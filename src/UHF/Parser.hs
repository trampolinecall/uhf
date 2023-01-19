{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module UHF.Parser
    ( parse

    , ParseError.ParseError
    , tests
    ) where

import UHF.Util.Prelude

import qualified UHF.Parser.PEG as Parser
import qualified UHF.Parser.ParseError as ParseError
import qualified UHF.Parser.Decl as Decl
import qualified UHF.Parser.Test as Test

import qualified UHF.IO.Location as Location
import qualified UHF.IO.File as File
import qualified UHF.IO.Location.SpanHelper as SpanHelper

import qualified UHF.Token as Token

import qualified UHF.AST as AST

import qualified Data.InfList as InfList

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

import qualified Control.Monad.Trans.State as State

parse :: [Token.LNormalToken] -> Token.LNormalToken -> ([ParseError.ParseError], [AST.Decl])
parse toks eof_tok =
    case runStateT parse' (toks InfList.+++ InfList.repeat eof_tok) of
        Parser.ParseResult (errs, Right (res, _)) -> (errs, res)
        Parser.ParseResult (errs, Left err) -> (errs ++ [err], [])

parse' :: Parser.Parser [AST.Decl]
parse' = Parser.star Decl.decl >>= \ ds -> Parser.consume "end of file" (Token.EOF ()) >> pure ds
-- tests {{{1
test_parsing :: [TestTree]
test_parsing = map Test.run_test $ concat [Decl.tests]

tests :: TestTree
tests = $(testGroupGenerator)
