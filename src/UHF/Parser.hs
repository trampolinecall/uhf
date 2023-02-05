{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module UHF.Parser
    ( parse

    , Error.BacktrackingError
    , Error.OtherError
    , tests
    ) where

import UHF.Util.Prelude

import qualified UHF.Parser.PEG as PEG
import qualified UHF.Parser.Error as Error
import qualified UHF.Parser.Decl as Decl
import qualified UHF.Parser.Test as Test

import qualified UHF.Token as Token
import qualified UHF.AST as AST

import qualified Data.InfList as InfList

parse :: [Token.LToken] -> Token.LToken -> ([Error.OtherError], [Error.BacktrackingError], [AST.Decl])
parse toks eof_tok =
    case PEG.run_parser parse' (toks InfList.+++ InfList.repeat eof_tok) of
        (other_errors, bt_errors, Just (Just res, _)) -> (other_errors, bt_errors, res)
        (other_errors, bt_errors, _) -> (other_errors, bt_errors, [])

parse' :: PEG.Parser [AST.Decl]
parse' = PEG.star Decl.decl >>= \ ds -> PEG.consume "end of file" (Token.EOF ()) >> pure (catMaybes ds)

-- tests {{{1
test_parsing :: [TestTree]
test_parsing = map Test.run_test Decl.tests

tests :: TestTree
tests = $(testGroupGenerator)
