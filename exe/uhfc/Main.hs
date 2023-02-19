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
        Right (Just (res@(_, _, graph_nodes, params, _))) -> -- putTextLn (show res) >>
            putTextLn (graph_to_dot graph_nodes params)
        Right Nothing -> pure () -- TODO: decide what should happen here
        Left diags -> mapM_ (Diagnostic.report IO.stderr) diags

-- TODO: put this somewhere else
graph_to_dot :: Arena.Arena (IR.GraphNode (IR.Type Void) Void) IR.GraphNodeKey -> Arena.Arena (IR.GraphParam (IR.Type Void))  IR.GraphParamKey -> Text
graph_to_dot nodes params =
    snd $ runWriter (
            tell "strict digraph {\n" >>
            tell "    node [shape=record];\n" >>
            tell "    subgraph cluster_params {\n" >>
            Arena.transform_with_keyM print_param_node params >>
            tell "    }\n" >>
            Arena.transform_with_keyM print_graph_node nodes >>
            tell "}\n"
        )
    where
        node_key_to_dot_id :: IR.GraphNodeKey -> Text
        node_key_to_dot_id key = "node" <> show (Arena.unmake_key key)

        param_key_to_dot_id :: IR.GraphParamKey -> Text
        param_key_to_dot_id key = "param" <> show (Arena.unmake_key key)

        print_param_node key _ =
            tell ("    " <> param_key_to_dot_id key <> " [label = \"<name> param\"]\n")

        print_graph_node cur_key node =
            let (name, graph_connections, param_connections) =
                    -- TODO: print types
                    case node of
                        IR.GraphNode'Int _ i -> ("int: " <> show i, [], [])
                        IR.GraphNode'Float _ f -> ("float: " <> show f, [], [])
                        IR.GraphNode'Bool _ b -> ("bool: " <> show b, [], [])
                        IR.GraphNode'Char _ c -> ("char: " <> show c, [], [])
                        IR.GraphNode'String _ s -> ("string: \\\"" <> s <> "\\\"", [], [])
                        IR.GraphNode'Tuple _ a b -> ("tuple", [("a", a), ("b", b)], [])

                        IR.GraphNode'Lambda _ param body -> ("lambda", [("body", body)], [("param", param)])
                        IR.GraphNode'Param _ param -> ("param", [], [("p", param)])

                        IR.GraphNode'Call _ callee arg -> ("call", [("callee", callee), ("arg", arg)], [])

                        IR.GraphNode'TupleDestructure1 _ tup -> ("tuple destructure 1", [("tuple", tup)], [])
                        IR.GraphNode'TupleDestructure2 _ tup -> ("tuple destructure 2", [("tuple", tup)], [])

                        IR.GraphNode'Poison _ void -> absurd void

                make_port (name, _) = "<" <> name <> ">" <> name
                ports = if null graph_connections && null param_connections
                        then ""
                        else "|{" <> Text.intercalate "|" (map make_port graph_connections ++ map make_port param_connections) <> "}"
                label = "{<name> " <> name <> ports <> "}"

                make_connection to_dot_id (this_port, other) = "    " <> node_key_to_dot_id cur_key <> ":" <> this_port <> " -> " <> to_dot_id other <> ":name;\n"

            in tell ("    " <> node_key_to_dot_id cur_key <> " [label = \"" <> label <> "\"];\n") >>
            mapM_ (tell . make_connection node_key_to_dot_id) graph_connections >>
            mapM_ (tell . make_connection param_key_to_dot_id) param_connections
