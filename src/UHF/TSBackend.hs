module UHF.TSBackend (lower) where

import UHF.Util.Prelude

import qualified Arena

import qualified Data.Text as Text
import qualified Data.FileEmbed as FileEmbed

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

-- TS things {{{1
runtime_code :: Text
runtime_code = $(FileEmbed.embedStringFile "data/ts_runtime.ts")

data TSDecl
data TSNominalType = TSNominalType'Data IR.NominalTypeKey Text -- TODO
data TSLambdaType = TSLambdaType IR.GraphNodeKey Type Type -- TODO: captures
data TSGraphNodeType = TSGraphNodeType IR.GraphNodeKey Type [(Text, Type)] Text
data TS = TS [TSDecl] [TSNominalType] [TSLambdaType] [TSGraphNodeType]

instance Semigroup TS where
    (TS d1 n1 l1 g1) <> (TS d2 n2 l2 g2) = TS (d1 <> d2) (n1 <> n2) (l1 <> l2) (g1 <> g2)
instance Monoid TS where
    mempty = TS mempty mempty mempty mempty

tell_nominal_type :: TSNominalType -> Writer TS ()
tell_nominal_type nt = tell $ TS [] [nt] [] []
tell_lambda_type :: TSLambdaType -> Writer TS ()
tell_lambda_type lt = tell $ TS [] [] [lt] []
tell_graph_node_type :: TSGraphNodeType -> Writer TS ()
tell_graph_node_type gnt = tell $ TS [] [] [] [gnt]

stringify_ts_decl :: TSDecl -> Text
stringify_ts_decl = todo

stringify_ts_nominal_type :: TSNominalType -> Text
stringify_ts_nominal_type (TSNominalType'Data key name)=
    "// data " <> name <> "\n"
        <> "class " <> mangle_nominal_type key <> " {\n"
        <> "}\n"

stringify_ts_lambda_type :: NominalTypeArena -> TSLambdaType -> Text
stringify_ts_lambda_type nts (TSLambdaType key arg result) =
    "class " <> mangle_graph_node_as_lambda key <> " implements Lambda<" <> refer_type nts arg <> ", " <> refer_type nts result <> "> {\n"
        <> "    call(arg: " <> refer_type nts arg <> "): " <> refer_type nts result <> " {\n"
        <> "        throw new Error('not implemented yet');\n"
        <> "    }\n"
        <> "}\n"

stringify_ts_graph_node_type :: NominalTypeArena -> TSGraphNodeType -> Text
stringify_ts_graph_node_type nts (TSGraphNodeType key ty fields evaluation) =
    "class Evaluator" <> mangle_graph_node key <> " implements Evaluator<" <> refer_type_raw nts ty <> "> {\n"
        <> Text.concat (map (\ (field_name, field_type) -> "    " <> field_name <> ": " <> refer_type nts field_type <> ";\n") fields)
        <> "    evaluate(): " <> refer_type_raw nts ty <> " {\n"
        <> "        " <> evaluation
        <> "    }\n"
        <> "}\n"
-- lowering {{{1
lower :: DeclArena -> NominalTypeArena -> GraphNodeArena -> GraphParamArena -> Text
lower decls nominal_types nodes params =
    let ((), TS ts_decls ts_nominal_types ts_lambda_types ts_graph_node_types) =
            runWriter (
                Arena.transform_with_keyM define_decl decls >>
                Arena.transform_with_keyM define_nominal_type nominal_types >>
                Arena.transform_with_keyM (define_graph_node nodes params) nodes >>
                pure ()
            )
    in runtime_code
        <> Text.concat (map stringify_ts_decl ts_decls)
        <> Text.concat (map stringify_ts_nominal_type ts_nominal_types)
        <> Text.concat (map (stringify_ts_lambda_type nominal_types) ts_lambda_types)
        <> Text.concat (map (stringify_ts_graph_node_type nominal_types) ts_graph_node_types)

define_decl :: IR.DeclKey -> Decl -> Writer TS ()
define_decl _ (IR.Decl'Module _) = pure ()
define_decl _ (IR.Decl'Type _) = pure ()

define_nominal_type :: IR.NominalTypeKey -> NominalType -> Writer TS ()
define_nominal_type key (IR.NominalType'Data name variants) = tell_nominal_type (TSNominalType'Data key name)
define_nominal_type _ (IR.NominalType'Synonym _ _) = pure ()

define_graph_node :: GraphNodeArena -> GraphParamArena -> IR.GraphNodeKey -> GraphNode -> Writer TS ()
define_graph_node _ _ key (IR.GraphNode'Int ty i) = tell_graph_node_type $ TSGraphNodeType key ty [] ("return " <> show i <> ";\n")
define_graph_node _ _ key (IR.GraphNode'Float ty (num :% denom)) = tell_graph_node_type $ TSGraphNodeType key ty [] ("return " <> show num <> " / " <> show denom <> ";\n")
define_graph_node _ _ key (IR.GraphNode'Bool ty b) = tell_graph_node_type $ TSGraphNodeType key ty [] ("return " <> (if b then "true" else "false") <> ";\n")
define_graph_node _ _ key (IR.GraphNode'Char ty c) = tell_graph_node_type $ TSGraphNodeType key ty [] ("return " <> show c <> ";\n")
define_graph_node _ _ key (IR.GraphNode'String ty s) = tell_graph_node_type $ TSGraphNodeType key ty [] ("return " <> show s <> ";\n")
define_graph_node nodes _ key (IR.GraphNode'Tuple ty a b) = tell_graph_node_type $ TSGraphNodeType key ty [("a", IR.graph_node_type $ Arena.get nodes a), ("b", IR.graph_node_type $ Arena.get nodes b)] ("return [this.a, this.b];\n")

define_graph_node nodes params key (IR.GraphNode'Lambda ty param body) = -- TODO: annotate with captures
    let (IR.GraphParam param_ty) = Arena.get params param
    in tell_lambda_type (TSLambdaType key param_ty (IR.graph_node_type $ Arena.get nodes body)) >>
    tell_graph_node_type (TSGraphNodeType key ty [] ("return new " <> mangle_graph_node_as_lambda key <> "();\n")) -- TODO
define_graph_node _ _ _ (IR.GraphNode'Param _ _) = pure () -- params do not get lowered and instead are replaced by lambda calls

define_graph_node nodes _ key (IR.GraphNode'Call ty c a) = tell_graph_node_type $ TSGraphNodeType key ty [("callee", IR.graph_node_type $ Arena.get nodes c), ("arg", IR.graph_node_type $ Arena.get nodes a)] ("return this.callee.get_value().call(this.arg).get_value();\n")

define_graph_node nodes _ key (IR.GraphNode'TupleDestructure1 ty tup) = tell_graph_node_type $ TSGraphNodeType key ty [("tup", IR.graph_node_type $ Arena.get nodes tup)] ("return this.tup.get_value()[0].get_value();\n")
define_graph_node nodes _ key (IR.GraphNode'TupleDestructure2 ty tup) = tell_graph_node_type $ TSGraphNodeType key ty [("tup", IR.graph_node_type $ Arena.get nodes tup)] ("return this.tup.get_value()[1].get_value();\n")

define_graph_node _ _ _ (IR.GraphNode'Poison _ void) = absurd void

refer_type_raw :: NominalTypeArena -> IR.Type Void -> Text
refer_type_raw nts (IR.Type'Nominal ntk) =
    case Arena.get nts ntk of
        IR.NominalType'Data _ _ -> mangle_nominal_type ntk
        IR.NominalType'Synonym _ expansion -> refer_type nts expansion

refer_type_raw _ (IR.Type'Int) = "number"
refer_type_raw _ (IR.Type'Float) = "number"
refer_type_raw _ (IR.Type'Char) = "char"
refer_type_raw _ (IR.Type'String) = "string"
refer_type_raw _ (IR.Type'Bool) = "bool"
refer_type_raw nts (IR.Type'Function a r) = "Lambda<" <> refer_type nts a <> ", " <> refer_type nts r <> ">"
refer_type_raw nts (IR.Type'Tuple a b) = "[" <> refer_type nts a <> ", " <> refer_type nts b <> "]"
refer_type_raw _ (IR.Type'Variable void) = absurd void

refer_type :: NominalTypeArena -> IR.Type Void -> Text
refer_type nts ty = "Thunk<" <> refer_type_raw nts ty <> ">"

-- mangling {{{2
-- TODO: better mangling and unified mangling for everything
mangle_nominal_type :: IR.NominalTypeKey -> Text
mangle_nominal_type key = "nominal_type" <> show (Arena.unmake_key key)

mangle_graph_node_as_lambda :: IR.GraphNodeKey -> Text
mangle_graph_node_as_lambda key = "graph_node_lambda" <> show (Arena.unmake_key key)

mangle_graph_node :: IR.GraphNodeKey -> Text
mangle_graph_node key = "graph_node" <> show (Arena.unmake_key key)
