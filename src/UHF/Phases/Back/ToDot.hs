module UHF.Phases.Back.ToDot (to_dot) where

import UHF.Util.Prelude

import qualified Arena

import qualified Data.Text as Text

import qualified UHF.Data.IR.BackendIR as BackendIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.ID as ID

type BackendIR bound_where captures dependencies = BackendIR.BackendIR bound_where captures dependencies Type Void
type Type = Type.Type Void

to_dot :: BackendIR bound_where captures dependencies -> Text
to_dot (BackendIR.BackendIR _ _ _ bindings params _) =
    snd $ runWriter (
            tell "strict digraph {\n" >>
            tell "    node [shape=record];\n" >>
            tell "    subgraph cluster_params {\n" >>
            Arena.transform_with_keyM print_param params >> -- TODO: do this by tracing from cu
            tell "    }\n" >>
            Arena.transform_with_keyM print_binding bindings >> -- TODO: same todo as above
            tell "}\n"
        )
    where
        binding_key_to_dot_id :: BackendIR.BindingKey -> Text
        binding_key_to_dot_id key = "binding" <> BackendIR.mangle_id (BackendIR.binding_id $ Arena.get bindings key)

        param_key_to_dot_id :: BackendIR.ParamKey -> Text
        param_key_to_dot_id key =
            let (BackendIR.Param id _) = Arena.get params key
            in "param" <> ID.mangle id

        print_param key _ =
            tell ("    " <> param_key_to_dot_id key <> " [label = \"<name> param\"]\n")

        stringify_matcher (BackendIR.Switch'BoolLiteral b)
            | b = "true"
            | otherwise = "false"
        stringify_matcher BackendIR.Switch'Tuple = "tuple"
        stringify_matcher BackendIR.Switch'Default = "_"

        print_binding cur_key (BackendIR.Binding _ _ initializer) =
            -- TODO: decide what to do with dependencies
            let (name, graph_connections, param_connections) =
                    -- TODO: print types
                    case initializer of
                        BackendIR.Expr'Refer _ _ b -> ("identifier", [("identifier", b)], [])

                        BackendIR.Expr'Int _ _ i -> ("int: " <> show i, [], [])
                        BackendIR.Expr'Float _ _ f -> ("float: " <> show f, [], [])
                        BackendIR.Expr'Bool _ _ b -> ("bool: " <> show b, [], [])
                        BackendIR.Expr'Char _ _ c -> ("char: " <> show c, [], [])
                        BackendIR.Expr'String _ _ s -> ("string: \\\"" <> s <> "\\\"", [], [])
                        BackendIR.Expr'Tuple _ _ a b -> ("tuple", [("a", a), ("b", b)], [])

                        BackendIR.Expr'Lambda _ _ param _ body -> ("lambda", [("body", body)], [("param", param)])
                        BackendIR.Expr'Param _ _ param -> ("param", [], [("p", param)])

                        BackendIR.Expr'Call _ _ callee arg -> ("call", [("callee", callee), ("arg", arg)], [])

                        BackendIR.Expr'Switch _ _ e arms -> ("switch", ("e", e) : zipWith (\ arm_i (matcher, _, result) -> (show arm_i <> " - " <> stringify_matcher matcher, result)) [0 :: Int ..] arms, [])

                        BackendIR.Expr'TupleDestructure1 _ _ tup -> ("tuple destructure 1", [("tuple", tup)], [])
                        BackendIR.Expr'TupleDestructure2 _ _ tup -> ("tuple destructure 2", [("tuple", tup)], [])

                        BackendIR.Expr'Forall _ _ _ _ e -> ("forall", [("e", e)], []) -- TODO: put vars
                        BackendIR.Expr'TypeApply _ _ e ty -> ("type apply", [("e", e)], []) -- TODO: put type

                        BackendIR.Expr'MakeADT _ _ _ args -> ("type apply", zipWith (\ i a -> ("arg" <> show (i :: Int), a)) [0..] args, []) -- TODO: connect to variant

                        BackendIR.Expr'Poison _ _ void -> absurd void

                make_port (name, _) = "<" <> name <> ">" <> name
                ports = if null graph_connections && null param_connections
                        then ""
                        else "|{" <> Text.intercalate "|" (map make_port graph_connections ++ map make_port param_connections) <> "}"
                label = "{<name> " <> name <> ports <> "}"

                make_connection to_dot_id (this_port, other) = "    " <> binding_key_to_dot_id cur_key <> ":\"" <> this_port <> "\" -> " <> to_dot_id other <> ":name;\n"

            in tell ("    " <> binding_key_to_dot_id cur_key <> " [label = \"" <> label <> "\"];\n") >>
            mapM_ (tell . make_connection binding_key_to_dot_id) graph_connections >>
            mapM_ (tell . make_connection param_key_to_dot_id) param_connections
