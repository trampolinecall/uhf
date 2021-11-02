module Main where

import System.Environment (getArgs, getProgName)

import Brace.ArgParser

args_description :: Description
args_description = Description
    [ positional "files to compile" "FILES" OneOrMore
    ]

main :: IO ()
main =
    getProgName >>= \ prog_name ->
    getArgs >>= \ args ->
    get_matches args_description prog_name args >>= \ matches ->
    report_errors (get_positional_result_ne "FILES" matches) >>= \ files ->
    putStrLn (show files)
