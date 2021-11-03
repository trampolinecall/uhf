module Main where

import Brace.ArgParser

args_description :: Description
args_description = Description
    [ positional "files to compile" "FILES" OneOrMore
    ]

main :: IO ()
main =
    get_matches_io args_description >>= \ matches ->
    report_errors (get_positional_result_ne "FILES" matches) >>= \ files ->
    putStrLn (show files)
