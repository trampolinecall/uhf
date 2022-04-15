module Main where

import UHF.ArgParser
import qualified UHF.Compiler as Compiler
import qualified UHF.IO.File as File
import qualified UHF.Diagnostic as Diagnostic

import qualified System.IO as IO

import qualified Data.List.NonEmpty as NonEmpty

args_description :: Description
args_description = Description
    [ positional "files to compile" "FILES" OneOrMore
    ]

main :: IO ()
main =
    get_matches_io args_description >>= \ matches ->
    report_errors (get_positional_result_ne "FILES" matches) >>= \ files ->
    sequence_ (NonEmpty.map compile files)

compile :: String -> IO ()
compile fname =
    File.open_file fname >>= \ f ->
    let (diags, res) = Compiler.compile f
    in Diagnostic.report_diagnostics IO.stderr diags >>
    return ()
    -- print res
