module UHF.Phases.Back.ToDot (to_dot) where

import UHF.Util.Prelude

import qualified Arena

import qualified Data.Text as Text

import qualified UHF.Data.IR.ANFIR as ANFIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.ID as ID

type ANFIR captures = ANFIR.ANFIR captures Type Void
type Type = Type.Type Void

to_dot :: ANFIR captures -> Text
to_dot (ANFIR.ANFIR _ _ _ _ bindings params _) =
    snd $ runWriter (
            tell "strict digraph {\n" >>
            tell "    node [shape=record];\n" >>
            tell "    subgraph cluster_params {\n" >>
            Arena.transform_with_keyM print_param params >> -- TODO: do this by tracing from module
            tell "    }\n" >>
            Arena.transform_with_keyM print_binding bindings >> -- TODO: same todo as above
            tell "}\n"
        )
    where
        binding_key_to_dot_id :: ANFIR.BindingKey -> Text
        binding_key_to_dot_id key = "binding" <> ANFIR.mangle_id (ANFIR.binding_id $ Arena.get bindings key)

        param_key_to_dot_id :: ANFIR.ParamKey -> Text
        param_key_to_dot_id key =
            let (ANFIR.Param id _) = Arena.get params key
            in "param" <> ID.mangle id

        print_param key _ =
            tell ("    " <> param_key_to_dot_id key <> " [label = \"<name> param\"]\n")

        stringify_matcher (ANFIR.Switch'BoolLiteral b)
            | b = "true"
            | otherwise = "false"
        stringify_matcher ANFIR.Switch'Tuple = "tuple"
        stringify_matcher ANFIR.Switch'Default = "_"

        print_binding cur_key (ANFIR.Binding _ initializer) =
            let (name, graph_connections, param_connections) =
                    -- TODO: print types
                    case initializer of
                        ANFIR.Expr'Refer _ _ b -> ("identifier", [("identifier", b)], [])

                        ANFIR.Expr'Int _ _ i -> ("int: " <> show i, [], [])
                        ANFIR.Expr'Float _ _ f -> ("float: " <> show f, [], [])
                        ANFIR.Expr'Bool _ _ b -> ("bool: " <> show b, [], [])
                        ANFIR.Expr'Char _ _ c -> ("char: " <> show c, [], [])
                        ANFIR.Expr'String _ _ s -> ("string: \\\"" <> s <> "\\\"", [], [])
                        ANFIR.Expr'Tuple _ _ a b -> ("tuple", [("a", a), ("b", b)], [])

                        ANFIR.Expr'Lambda _ _ param _ body -> ("lambda", [("body", body)], [("param", param)])
                        ANFIR.Expr'Param _ _ param -> ("param", [], [("p", param)])

                        ANFIR.Expr'Call _ _ callee arg -> ("call", [("callee", callee), ("arg", arg)], [])

                        ANFIR.Expr'Switch _ _ e arms -> ("switch", ("e", e) : zipWith (\ arm_i (matcher, _, result) -> (show arm_i <> " - " <> stringify_matcher matcher, result)) [0 :: Int ..] arms, [])

                        ANFIR.Expr'Seq _ _ a b -> ("seq", [("a", a), ("b", b)], [])

                        ANFIR.Expr'TupleDestructure1 _ _ tup -> ("tuple destructure 1", [("tuple", tup)], [])
                        ANFIR.Expr'TupleDestructure2 _ _ tup -> ("tuple destructure 2", [("tuple", tup)], [])

                        ANFIR.Expr'Forall _ _ _ _ e -> ("forall", [("e", e)], []) -- TODO: put vars
                        ANFIR.Expr'TypeApply _ _ e ty -> ("type apply", [("e", e)], []) -- TODO: put type

                        ANFIR.Expr'MakeADT _ _ _ args -> ("type apply", zipWith (\ i a -> ("arg" <> show (i :: Int), a)) [0..] args, []) -- TODO: connect to variant

                        ANFIR.Expr'Poison _ _ void -> absurd void

                make_port (name, _) = "<" <> name <> ">" <> name
                ports = if null graph_connections && null param_connections
                        then ""
                        else "|{" <> Text.intercalate "|" (map make_port graph_connections ++ map make_port param_connections) <> "}"
                label = "{<name> " <> name <> ports <> "}"

                make_connection to_dot_id (this_port, other) = "    " <> binding_key_to_dot_id cur_key <> ":\"" <> this_port <> "\" -> " <> to_dot_id other <> ":name;\n"

            in tell ("    " <> binding_key_to_dot_id cur_key <> " [label = \"" <> label <> "\"];\n") >>
            mapM_ (tell . make_connection binding_key_to_dot_id) graph_connections >>
            mapM_ (tell . make_connection param_key_to_dot_id) param_connections
