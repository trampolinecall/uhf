module UHF.ToDot (to_dot) where

import UHF.Util.Prelude

import qualified Arena

import qualified Data.Text as Text

import qualified UHF.IR as IR

type Decl = IR.Decl
type DeclArena = Arena.Arena Decl IR.DeclKey

type Type = IR.Type Void
type NominalType = IR.NominalType Type
type GraphNode = IR.GraphNode Type Void
type GraphParam = IR.GraphParam Type

type NominalTypeArena = Arena.Arena NominalType IR.NominalTypeKey
type GraphNodeArena = Arena.Arena GraphNode IR.GraphNodeKey
type GraphParamArena = Arena.Arena GraphParam IR.GraphParamKey

to_dot :: DeclArena -> NominalTypeArena -> GraphNodeArena -> GraphParamArena -> Text
to_dot decls nominal_types nodes params =
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
