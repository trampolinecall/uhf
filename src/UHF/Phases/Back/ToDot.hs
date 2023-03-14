module UHF.Phases.Back.ToDot (to_dot) where

import UHF.Util.Prelude

import qualified Arena

import qualified Data.Text as Text

import qualified UHF.Data.IR.ANFIR as ANFIR
import qualified UHF.Data.IR.Type as Type

type ANFIR = ANFIR.ANFIR Type Void
type Type = Type.Type Void

to_dot :: ANFIR -> Text
to_dot (ANFIR.ANFIR _ _ _ nodes params _) =
    snd $ runWriter (
            tell "strict digraph {\n" >>
            tell "    node [shape=record];\n" >>
            tell "    subgraph cluster_params {\n" >>
            Arena.transform_with_keyM print_param params >> -- TODO: do this by tracing from module
            tell "    }\n" >>
            Arena.transform_with_keyM print_binding nodes >> -- TODO: same todo as above
            tell "}\n"
        )
    where
        binding_key_to_dot_id :: ANFIR.BindingKey -> Text
        binding_key_to_dot_id key = "node" <> show (Arena.unmake_key key)

        param_key_to_dot_id :: ANFIR.ParamKey -> Text
        param_key_to_dot_id key = "param" <> show (Arena.unmake_key key)

        print_param key _ =
            tell ("    " <> param_key_to_dot_id key <> " [label = \"<name> param\"]\n")

        stringify_matcher (ANFIR.Switch'BoolLiteral b)
            | b = "true"
            | otherwise = "false"
        stringify_matcher ANFIR.Switch'Tuple = "tuple"
        stringify_matcher ANFIR.Switch'Default = "_"

        print_binding cur_key (ANFIR.Binding initializer) =
            let (name, graph_connections, param_connections) =
                    -- TODO: print types
                    case initializer of
                        ANFIR.Expr'Identifier _ b -> ("identifier", [("identifier", b)], [])

                        ANFIR.Expr'Int _ i -> ("int: " <> show i, [], [])
                        ANFIR.Expr'Float _ f -> ("float: " <> show f, [], [])
                        ANFIR.Expr'Bool _ b -> ("bool: " <> show b, [], [])
                        ANFIR.Expr'Char _ c -> ("char: " <> show c, [], [])
                        ANFIR.Expr'String _ s -> ("string: \\\"" <> s <> "\\\"", [], [])
                        ANFIR.Expr'Tuple _ a b -> ("tuple", [("a", a), ("b", b)], [])

                        ANFIR.Expr'Lambda _ _ param _ body -> ("lambda", [("body", body)], [("param", param)])
                        ANFIR.Expr'Param _ param -> ("param", [], [("p", param)])

                        ANFIR.Expr'Call _ callee arg -> ("call", [("callee", callee), ("arg", arg)], [])

                        ANFIR.Expr'Switch _ e arms -> ("switch", ("e", e) : zipWith (\ arm_i (matcher, result) -> (show arm_i <> " - " <> stringify_matcher matcher, result)) [0 :: Int ..] arms, [])

                        ANFIR.Expr'Seq _ a b -> ("seq", [("a", a), ("b", b)], [])

                        ANFIR.Expr'TupleDestructure1 _ tup -> ("tuple destructure 1", [("tuple", tup)], [])
                        ANFIR.Expr'TupleDestructure2 _ tup -> ("tuple destructure 2", [("tuple", tup)], [])

                        ANFIR.Expr'Poison _ void -> absurd void

                make_port (name, _) = "<" <> name <> ">" <> name
                ports = if null graph_connections && null param_connections
                        then ""
                        else "|{" <> Text.intercalate "|" (map make_port graph_connections ++ map make_port param_connections) <> "}"
                label = "{<name> " <> name <> ports <> "}"

                make_connection to_dot_id (this_port, other) = "    " <> binding_key_to_dot_id cur_key <> ":\"" <> this_port <> "\" -> " <> to_dot_id other <> ":name;\n"

            in tell ("    " <> binding_key_to_dot_id cur_key <> " [label = \"" <> label <> "\"];\n") >>
            mapM_ (tell . make_connection binding_key_to_dot_id) graph_connections >>
            mapM_ (tell . make_connection param_key_to_dot_id) param_connections
