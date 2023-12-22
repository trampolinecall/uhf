{-# LANGUAGE ExistentialQuantification #-}

module UHF.Parts.Parser.Test (ParsingTest(..), make_token_stream, run_test) where

import UHF.Prelude

import qualified Data.InfList as InfList

import qualified UHF.Data.Token as Token
import qualified UHF.Source.Located as Located
import qualified UHF.Parts.Parser.PEG as PEG

data ParsingTest = forall r. (Show r, Eq r) => ParsingTest [Char] (IO PEG.TokenStream) r [([Char], PEG.Parser r)]

make_token_stream :: [IO Token.Token] -> IO PEG.TokenStream
make_token_stream toks =
    sequence toks >>= \ toks ->
    mapM Located.dummy_locate toks >>=  \ toks ->
    let l = last toks
    in pure (InfList.zip (InfList.iterate (1+) 0) (toks InfList.+++ InfList.repeat l))

{- TODO:
check_parser :: [Error.Error] -> r -> [Token.Token] -> Parser r -> PEG.TokenStream -> IO ()
check_parser expected_recoverable_errors expected_result parser toks =
    let PEG.ParseResult (result_recoverable_errors, result) = runStateT parser toks
    in result_recoverable_errors @?= expected_recoverable_errors >>
    case (result, expected_result) of
        (Right (got_result, got_toks'), (expected_result, expected_toks'))

        _ -> assertFailure "checking parser failed: constructors for result do not match"
-}

run_test :: ParsingTest -> TestTree
run_test (ParsingTest construct_name input_toks expected_res parsers) =
    testGroup construct_name $
        map
            (\ (p_name, p) ->
                testCase (p_name ++ " parsing " ++ construct_name) $
                    input_toks >>= \ input_toks ->
                    case PEG.eval_parser p input_toks of
                        (bt_errors, res) ->
                            if Just expected_res == res
                                then pure ()
                                else assertFailure $ "parse test failed: got errors " ++ show bt_errors ++ ", and result " ++ show res ++ " but expected " ++ show expected_res)
            parsers
