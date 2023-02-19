{-# LANGUAGE FlexibleContexts #-}

module Main where

import UHF.Util.Prelude

import Options.Applicative

import qualified Driver

import qualified Data.Text as Text

import qualified System.IO as IO
import UHF.IO.Location (open_file) -- TODO: rename this module to something better (open_file in the Location module is a bit weird)

import qualified Arena

import qualified UHF.Diagnostic as Diagnostic

import qualified UHF.IR as IR

newtype Args = Args [FilePath]

argparser :: ParserInfo Args
argparser = info (args <**> helper)
    ( fullDesc
    <> progDesc "compile one or more uhf files"
    <> header "uhfc: uhf compiler"
    )
    where
        args = Args <$>
            some (
                argument str
                    ( metavar "FILES..."
                    <> help "files to compile"
                    )
            )

main :: IO ()
main =
    execParser argparser >>= \ (Args files) ->
    let total_files = length files
    in mapM_ (\ (num, f) -> compile num total_files f) (zip [1..] files)

compile :: Int -> Int -> FilePath -> IO ()
compile num total fname =
    open_file fname >>= \ f ->
    -- putStrLn ("[" <> show num <> "/" <> show total <> "]: compiling " <> format f) >>
    case Driver.compile f of
        Right res@(_, _, graph, _) -> -- putTextLn (show res) >>
            putTextLn (graph_to_dot graph)
        Left diags -> mapM_ (Diagnostic.report IO.stderr) diags

-- TODO: put this somewhere else
graph_to_dot :: Arena.Arena IR.GraphNode IR.GraphNodeKey -> Text
graph_to_dot nodes =
    snd $ runWriter (tell "strict digraph {\n    node [shape=record];\n" >> Arena.transform_with_keyM print_node nodes >> tell "}\n")
    where
        key_to_dot_id key = "node" <> show (Arena.unmake_key key)
        print_node node_key node =
            let (label, connections) =
                    -- TODO: print types
                    case node of
                        IR.GraphNode'Int _ i -> ("int: " <> show i, [])
                        IR.GraphNode'Float _ f -> ("float: " <> show f, [])
                        IR.GraphNode'Bool _ b -> ("bool: " <> show b, [])
                        IR.GraphNode'Char _ c -> ("char: " <> show c, [])
                        IR.GraphNode'String _ s -> ("string: \\\"" <> s <> "\\\"", [])
                        IR.GraphNode'Tuple _ a b -> ("tuple", [("a", a), ("b", b)])

                        IR.GraphNode'Param _ -> ("param", [])
                        IR.GraphNode'Lambda _ param body -> ("lambda", [("param", param), ("body", body)])

                        IR.GraphNode'Call _ callee arg -> ("call", [("callee", callee), ("arg", arg)])

                        IR.GraphNode'TupleDestructure1 _ tup -> ("tuple destructure 1", [("tuple", tup)])
                        IR.GraphNode'TupleDestructure2 _ tup -> ("tuple destructure 2", [("tuple", tup)])

                        IR.GraphNode'Poison _ -> ("poison", [])

            in tell ("    " <> key_to_dot_id node_key <> " [label = \"{<name> " <> label <> "|{" <> Text.intercalate "|" (map (\ (port, _) -> "<" <> port <> ">" <> port) connections) <> "}}\"]" <> ";\n") >>
            mapM_ (\ (this_port, other) -> tell $ "    " <> key_to_dot_id node_key <> ":" <> this_port <> " -> " <> key_to_dot_id other <> ":name;\n") connections
