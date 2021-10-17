{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Testing
    ( TestSuite(..)
    , Test(..)
    , TestResult(..)
    , run_test_suite

    , pass_test
    , fail_test
    , pending_test
    , pass_if
    ) where

-- inspired by RSpec

import System.Exit (exitFailure)

data TestSuite = TestSuite [Test]
data Test
    = DescribeModule String [Test]
    | DescribeFunction String [Test]
    | When String [Test]
    | ItCan String (IO TestResult)

data TestResult = Pass | Fail | TestPending

run_test_suite :: TestSuite -> IO ()
run_test_suite (TestSuite tests) =
    run_test_list 0 tests >>= \ results ->
    let count b = length $ filter b results

        match_pass = \case
            Pass -> True
            _ -> False

        match_fail = \case
            Fail -> True
            _ -> False

        match_pending = \case
            TestPending -> True
            _ -> False

        num_pass = count match_pass
        num_fail = count match_fail
        num_pending = count match_pending

    in putStrLn (show num_pass ++ " tests passed") >>
    putStrLn (show num_fail ++ " tests failed") >>
    putStrLn (show num_pending ++ " tests pending implementation") >>
    if (num_fail > 0) then exitFailure else return ()

run_test :: Int -> Test -> IO [TestResult]

run_test indent_amt (DescribeModule name test_list) = heading_and_children indent_amt ("module " ++ name ++ ":\n") test_list
run_test indent_amt (DescribeFunction name test_list) = heading_and_children indent_amt ("function '" ++ name ++ "':\n") test_list
run_test indent_amt (When context test_list) = heading_and_children indent_amt ("when " ++ context ++ ":\n") test_list

run_test indent_amt (ItCan action result) =
    indent_put_str indent_amt "it " >>
    result >>= \ result' ->
    (case result' of
        Pass -> putStr $ "can " ++ action
        Fail -> putStr $ "CANNOT " ++ action ++ "!"
        TestPending -> putStr $ "maybe can " ++ action
    ) >>
    putStr "\n" >>
    return [result']

heading_and_children :: Int -> String -> [Test] -> IO [TestResult]
heading_and_children indent_amt msg test_list = indent_put_str indent_amt msg >> run_test_list (indent_amt + 4) test_list

run_test_list :: Int -> [Test] -> IO [TestResult]
run_test_list indent_amt test_list = concat <$> mapM (run_test indent_amt) test_list

indent :: Int -> String
indent = (`replicate` ' ')

indent_put_str :: Int -> String -> IO ()
indent_put_str amt = putStr . (indent amt ++)

pass_test, fail_test, pending_test :: IO TestResult
pass_test = return Pass
fail_test = return Fail
pending_test = return TestPending

pass_if :: Bool -> IO TestResult
pass_if b
    | b = pass_test
    | otherwise = fail_test
