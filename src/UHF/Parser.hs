{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module UHF.Parser
    ( parse

    , Error.BacktrackingError
    , Error.OtherError
    , tests
    ) where

import UHF.Util.Prelude

import qualified UHF.IO.Location as Location

import qualified UHF.Parser.PEG as PEG
import qualified UHF.Parser.Error as Error
import qualified UHF.Parser.Decl as Decl
import qualified UHF.Parser.Test as Test

import qualified UHF.Token as Token
import qualified UHF.AST as AST

import qualified Data.InfList as InfList

parse :: [Token.LToken] -> Token.LToken -> ([Error.OtherError], Maybe (Location.Located [Error.BacktrackingError]), [AST.Decl])
parse toks eof_tok =
    case PEG.run_parser parse' (InfList.zip (InfList.iterate (1+) 0) (toks InfList.+++ InfList.repeat eof_tok)) of
        (other_errors, _, Just (res, _)) -> (other_errors, Nothing, res)
        (other_errors, bt_errors, _) -> (other_errors, choose_error bt_errors, [])

parse' :: PEG.Parser [AST.Decl]
parse' = PEG.star Decl.decl >>= \ ds -> PEG.consume "end of file" (Token.EOF ()) >> pure ds

choose_error :: [Error.BacktrackingError] -> Maybe (Location.Located [Error.BacktrackingError])
choose_error [] = Nothing
choose_error errs =
    let max_ind = maximum $ map (\ (Error.BadToken ind _ _ _) -> ind) errs
        latest_errors = filter (\ (Error.BadToken ind _ _ _) -> ind == max_ind) errs
        (Error.BadToken _ (Location.Located latest_span _) _ _) = head latest_errors
    in Just $ Location.Located latest_span latest_errors

-- tests {{{1
test_parsing :: [TestTree]
test_parsing = map Test.run_test Decl.tests

tests :: TestTree
tests = $(testGroupGenerator)
