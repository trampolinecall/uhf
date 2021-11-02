module Main where

import System.Environment (getArgs, getProgName)

import Brace.ArgParser

args_description :: Description
args_description = Description
    [ positional "file to run" "FILE" (Number 1)
    ]

main :: IO ()
main =
    getProgName >>= \ prog_name ->
    getArgs >>= \ args ->
    get_matches args_description prog_name args >>= \ matches ->
    report_errors (get_positional_result_1 "FILE" matches) >>= \ file ->
    putStrLn (show file)
