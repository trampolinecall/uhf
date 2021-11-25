module Main where

import UHF.ArgParser

args_description :: Description
args_description = Description
    [ positional "file to run" "FILE" (Number 1)
    ]

main :: IO ()
main =
    get_matches_io args_description >>= \ matches ->
    report_errors (get_positional_result_1 "FILE" matches) >>= \ file ->
    putStrLn (show file)
