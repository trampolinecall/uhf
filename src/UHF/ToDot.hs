module UHF.ToDot (to_dot) where

import UHF.Util.Prelude

import qualified Arena

import qualified Data.Text as Text

import qualified UHF.HIR as HIR
import qualified UHF.ANFIR as ANFIR

type Decl = ANFIR.Decl
type DeclArena = Arena.Arena Decl HIR.DeclKey

type Type = HIR.Type Void
type NominalType = HIR.NominalType Type
type GraphNode = ANFIR.Node Type Void
type GraphParam = ANFIR.Param Type

type NominalTypeArena = Arena.Arena NominalType HIR.NominalTypeKey
type GraphNodeArena = Arena.Arena GraphNode ANFIR.NodeKey
type GraphParamArena = Arena.Arena GraphParam ANFIR.ParamKey

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
        node_key_to_dot_id :: ANFIR.NodeKey -> Text
        node_key_to_dot_id key = "node" <> show (Arena.unmake_key key)

        param_key_to_dot_id :: ANFIR.ParamKey -> Text
        param_key_to_dot_id key = "param" <> show (Arena.unmake_key key)

        print_param_node key _ =
            tell ("    " <> param_key_to_dot_id key <> " [label = \"<name> param\"]\n")

        print_graph_node cur_key node =
            let (name, graph_connections, param_connections) =
                    -- TODO: print types
                    case node of
                        ANFIR.Node'Int _ i -> ("int: " <> show i, [], [])
                        ANFIR.Node'Float _ f -> ("float: " <> show f, [], [])
                        ANFIR.Node'Bool _ b -> ("bool: " <> show b, [], [])
                        ANFIR.Node'Char _ c -> ("char: " <> show c, [], [])
                        ANFIR.Node'String _ s -> ("string: \\\"" <> s <> "\\\"", [], [])
                        ANFIR.Node'Tuple _ a b -> ("tuple", [("a", a), ("b", b)], [])

                        ANFIR.Node'Lambda _ param _ body -> ("lambda", [("body", body)], [("param", param)])
                        ANFIR.Node'Param _ param -> ("param", [], [("p", param)])

                        ANFIR.Node'Call _ callee arg -> ("call", [("callee", callee), ("arg", arg)], [])

                        ANFIR.Node'TupleDestructure1 _ tup -> ("tuple destructure 1", [("tuple", tup)], [])
                        ANFIR.Node'TupleDestructure2 _ tup -> ("tuple destructure 2", [("tuple", tup)], [])

                        ANFIR.Node'Poison _ void -> absurd void

                make_port (name, _) = "<" <> name <> ">" <> name
                ports = if null graph_connections && null param_connections
                        then ""
                        else "|{" <> Text.intercalate "|" (map make_port graph_connections ++ map make_port param_connections) <> "}"
                label = "{<name> " <> name <> ports <> "}"

                make_connection to_dot_id (this_port, other) = "    " <> node_key_to_dot_id cur_key <> ":" <> this_port <> " -> " <> to_dot_id other <> ":name;\n"

            in tell ("    " <> node_key_to_dot_id cur_key <> " [label = \"" <> label <> "\"];\n") >>
            mapM_ (tell . make_connection node_key_to_dot_id) graph_connections >>
            mapM_ (tell . make_connection param_key_to_dot_id) param_connections
