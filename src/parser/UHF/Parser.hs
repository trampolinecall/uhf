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

import qualified UHF.Token as Token
import qualified UHF.AST as AST

import qualified Data.InfList as InfList

parse :: [Token.LToken] -> Token.LToken -> ([ParseError.ParseError], [AST.Decl])
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
