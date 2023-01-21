{-# LANGUAGE ExistentialQuantification #-}

module UHF.Parser.Test (ParsingTest(..), make_token_stream, run_test) where

import UHF.Util.Prelude

import qualified UHF.Parser.PEG as PEG
import qualified UHF.Parser.ParseError as ParseError

import qualified UHF.IO.Location as Location
import qualified UHF.IO.File as File
import qualified UHF.IO.Location.SpanHelper as SpanHelper

import qualified UHF.Token as Token

import qualified UHF.AST as AST

import qualified Data.InfList as InfList

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

import qualified Control.Monad.Trans.State as State

data ParsingTest = forall r. (Show r, Eq r) => ParsingTest [Char] (File.File, PEG.TokenStream) r [([Char], PEG.Parser r)]

make_token_stream :: [(Text, Token.NormalToken)] -> (File.File, PEG.TokenStream)
make_token_stream things =
    let (file, things') = SpanHelper.make_spans_with_items things
        l = last things'
    in (file, things' InfList.+++ InfList.repeat l)

{- TODO:
check_parser :: [ParseError.ParseError] -> r -> [Token.Token] -> Parser r -> PEG.TokenStream -> IO ()
check_parser expected_recoverable_errors expected_result parser toks =
    let PEG.ParseResult (result_recoverable_errors, result) = runStateT parser toks
    in result_recoverable_errors @?= expected_recoverable_errors >>
    case (result, expected_result) of
        (Right (got_result, got_toks'), (expected_result, expected_toks'))

        _ -> assertFailure "checking parser failed: constructors for result do not match"
-}

run_test :: ParsingTest -> TestTree
run_test (ParsingTest construct_name (_, construct_toks) construct_res parsers) =
    testGroup construct_name $
        map
            (\ (p_name, p) ->
                testCase (p_name ++ " parsing " ++ construct_name) $ PEG.ParseResult ([], Right construct_res) @=? evalStateT p construct_toks)
            parsers
