module UHF.TSBackend (lower) where

import UHF.Util.Prelude

import qualified Arena

import qualified Data.Text as Text
import qualified Data.Set as Set
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

-- TODO: refactor everything
-- TODO: this actually does not work correctly like at all

type IRReader = Reader (NominalTypeArena, GraphNodeArena, GraphParamArena)

get_node :: IR.GraphNodeKey -> IRReader GraphNode
get_node k = reader (\ (_, a, _) -> Arena.get a k)
get_param :: IR.GraphParamKey -> IRReader GraphParam
get_param k = reader (\ (_, _, a) -> Arena.get a k)
get_nominal_type :: IR.NominalTypeKey -> IRReader NominalType
get_nominal_type k = reader (\ (a, _, _) -> Arena.get a k)

node_type :: IR.GraphNodeKey -> IRReader Type
node_type k = IR.graph_node_type <$> get_node k

-- TS things {{{1
runtime_code :: Text
runtime_code = $(FileEmbed.embedStringFile "data/ts_runtime.ts")

data TSDecl
data TSNominalType = TSNominalType'Data IR.NominalTypeKey Text -- TODO
data TSLambda = TSLambda IR.GraphNodeKey Type Type IR.GraphNodeKey -- TODO: captures
data TSEvaluator = TSEvaluator IR.GraphNodeKey Type [(Text, Type)] Text
data TSMakeThunkGraph = TSMakeThunkGraph IR.GraphNodeKey (Set IR.GraphNodeKey) (Set IR.GraphParamKey)
data TS = TS [TSDecl] [TSNominalType] [TSMakeThunkGraph] [TSLambda] [TSEvaluator]

instance Semigroup TS where
    (TS d1 m1 n1 l1 g1) <> (TS d2 m2 n2 l2 g2) = TS (d1 <> d2) (m1 <> m2) (n1 <> n2) (l1 <> l2) (g1 <> g2)
instance Monoid TS where
    mempty = TS mempty mempty mempty mempty mempty

type TSWriter = WriterT TS IRReader

tell_nominal_type :: TSNominalType -> TSWriter ()
tell_nominal_type nt = tell $ TS [] [nt] [] [] []
tell_make_thunk_graph :: TSMakeThunkGraph -> TSWriter ()
tell_make_thunk_graph mtg = tell $ TS [] [] [mtg] [] []
tell_lambda :: TSLambda -> TSWriter ()
tell_lambda lt = tell $ TS [] [] [] [lt] []
tell_evaluator :: TSEvaluator -> TSWriter ()
tell_evaluator gnt = tell $ TS [] [] [] [] [gnt]

stringify_ts_decl :: TSDecl -> IRReader Text
stringify_ts_decl = error "unreachable"

stringify_ts_nominal_type :: TSNominalType -> IRReader Text
stringify_ts_nominal_type (TSNominalType'Data key name) =
    pure $
        "// data " <> name <> "\n"
            <> "class " <> mangle_nominal_type key <> " {\n"
            <> "}\n"

stringify_ts_make_thunk_graph :: TSMakeThunkGraph -> IRReader Text
stringify_ts_make_thunk_graph (TSMakeThunkGraph main_key included_nodes included_params) =
    node_type main_key >>= refer_type >>= \ main_node_type ->
    (mapM stringify_param (Set.toList included_params) :: IRReader [Text]) >>= \ params_stringified ->

    concat <$> mapM stringify_node_decl (Set.toList included_nodes) >>= \ node_decls ->
    concat <$> mapM stringify_node_set_fields (Set.toList included_nodes) >>= \ node_set_fields ->
    -- TODO: dont use GraphParamKey Ord instance to decide parameter order (not deterministic across compiles)
    -- also TODO: dont use unmake_key anywhere (probably including outside of this module too)

    pure ("function " <> mangle_graph_node_as_make_thunk_graph main_key <> "(" <> Text.intercalate ", " params_stringified <> "): " <> main_node_type <> " {\n" -- TODO: captures
        <> Text.unlines node_decls
        <> Text.unlines node_set_fields
        <> "    return " <> mangle_graph_node_as_local_thunk main_key <> ";\n"
        <> "}\n")
    where
        mangle_graph_node_as_local_thunk, mangle_graph_node_as_local_evaluator :: IR.GraphNodeKey -> Text
        mangle_graph_node_as_local_thunk key = "thunk" <> show (Arena.unmake_key key)
        mangle_graph_node_as_local_evaluator key = "evaluator" <> show (Arena.unmake_key key)

        stringify_param param_key =
            get_param param_key >>= \ (IR.GraphParam param_ty) ->
            refer_type param_ty >>= \ ty_refer ->
            pure ("param_" <> show (Arena.unmake_key param_key) <> ": " <> ty_refer)

        stringify_node_decl node_key =
            node_type node_key >>= refer_type >>= \ cur_node_type ->
            let let_evaluator evaluator_args = "let " <> mangle_graph_node_as_local_evaluator node_key <> ": " <> mangle_graph_node_as_evaluator node_key <> " = " <> "new " <> mangle_graph_node_as_evaluator node_key <> "(" <> evaluator_args <> ")" <> ";"
                let_thunk :: Text -> Text
                let_thunk initializer = "let " <> mangle_graph_node_as_local_thunk node_key <> ": " <> (cur_node_type :: Text) <> " = " <> initializer <> ";"
                default_let_thunk = let_thunk ("new Thunk(" <> mangle_graph_node_as_local_evaluator node_key <> ")")

            in get_node node_key >>= \case
                IR.GraphNode'Int _ _ -> pure [let_evaluator "", default_let_thunk]
                IR.GraphNode'Float _ _ -> pure [let_evaluator "", default_let_thunk]
                IR.GraphNode'Bool _ _ -> pure [let_evaluator "", default_let_thunk]
                IR.GraphNode'Char _ _ -> pure [let_evaluator "", default_let_thunk]
                IR.GraphNode'String _ _ -> pure [let_evaluator "", default_let_thunk]
                IR.GraphNode'Tuple _ _ _ -> pure [let_evaluator "undefined, undefined", default_let_thunk]

                IR.GraphNode'Lambda _ _ _ -> pure [let_evaluator "", default_let_thunk] -- TODO: put captures here
                IR.GraphNode'Param _ param_key -> pure [let_thunk $ "param_" <> show (Arena.unmake_key param_key)]

                IR.GraphNode'Call _ _ _ -> pure [let_evaluator "undefined, undefined", default_let_thunk]

                IR.GraphNode'TupleDestructure1 _ _ -> pure [let_evaluator "undefined", default_let_thunk]
                IR.GraphNode'TupleDestructure2 _ _ -> pure [let_evaluator "undefined", default_let_thunk]

                IR.GraphNode'Poison _ void -> absurd void

        stringify_node_set_fields node_key =
            let set_field field other_node = mangle_graph_node_as_local_evaluator node_key <> "." <> field <> " = " <> mangle_graph_node_as_local_thunk other_node <> ";"
            in get_node node_key >>= \case
                IR.GraphNode'Int _ _ -> pure []
                IR.GraphNode'Float _ _ -> pure []
                IR.GraphNode'Bool _ _ -> pure []
                IR.GraphNode'Char _ _ -> pure []
                IR.GraphNode'String _ _ -> pure []
                IR.GraphNode'Tuple _ a b -> pure [set_field "a" a, set_field "b" b]

                IR.GraphNode'Lambda _ _ _ -> pure []
                IR.GraphNode'Param _ _ -> pure []

                IR.GraphNode'Call _ callee arg -> pure [set_field "callee" callee, set_field "arg" arg]

                IR.GraphNode'TupleDestructure1 _ tup -> pure [set_field "tup" tup]
                IR.GraphNode'TupleDestructure2 _ tup -> pure [set_field "tup" tup]

                IR.GraphNode'Poison _ void -> absurd void

stringify_ts_lambda :: TSLambda -> IRReader Text
stringify_ts_lambda (TSLambda key arg result body_key) =
    refer_type arg >>= \ arg_type ->
    refer_type result >>= \ result_type ->
    pure ("class " <> mangle_graph_node_as_lambda key <> " implements Lambda<" <> arg_type <> ", " <> result_type <> "> {\n"
        <> "    call(arg: " <> arg_type <> "): " <> result_type <> " {\n"
        <> "        return " <> mangle_graph_node_as_make_thunk_graph body_key <> "();\n" -- TODO: captures
        <> "    }\n"
        <> "}\n")

stringify_ts_evaluator :: TSEvaluator -> IRReader Text
stringify_ts_evaluator (TSEvaluator key ty fields evaluation) =
    refer_type_raw ty >>= \ ty ->
    Text.intercalate ", " <$> mapM (\ (field_name, field_type) -> refer_type field_type >>= \ field_type -> pure ("public " <> field_name <> ": " <> field_type)) fields >>= \ constructor_params ->
    pure ("class " <> mangle_graph_node_as_evaluator key <> " implements Evaluator<" <> ty <> "> {\n"
        <> "    constructor(" <> constructor_params <> ") {}\n"
        <> "    evaluate(): " <> ty <> " {\n"
        <> "        " <> evaluation
        <> "    }\n"
        <> "}\n")

-- referring to types {{{2
refer_type_raw :: IR.Type Void -> IRReader Text
refer_type_raw (IR.Type'Nominal ntk) =
    get_nominal_type ntk >>= \case
        IR.NominalType'Data _ _ -> pure $ mangle_nominal_type ntk
        IR.NominalType'Synonym _ expansion -> refer_type expansion

refer_type_raw IR.Type'Int = pure "number"
refer_type_raw IR.Type'Float = pure "number"
refer_type_raw IR.Type'Char = pure "char"
refer_type_raw IR.Type'String = pure "string"
refer_type_raw IR.Type'Bool = pure "bool"
refer_type_raw (IR.Type'Function a r) = refer_type a >>= \ a -> refer_type r >>= \ r -> pure ("Lambda<" <> a <> ", " <> r <> ">")
refer_type_raw (IR.Type'Tuple a b) = refer_type a >>= \ a -> refer_type b >>= \ b -> pure ("[" <> a <> ", " <> b <> "]")
refer_type_raw (IR.Type'Variable void) = absurd void

refer_type :: IR.Type Void -> IRReader Text
refer_type ty = refer_type_raw ty >>= \ ty -> pure ("Thunk<" <> ty <> ">")

-- lowering {{{1
lower :: DeclArena -> NominalTypeArena -> GraphNodeArena -> GraphParamArena -> Text
lower decls nominal_types nodes params =
    runReader
        (
            runWriterT (
                Arena.transform_with_keyM define_decl decls >>
                Arena.transform_with_keyM define_nominal_type nominal_types >>
                Arena.transform_with_keyM define_graph_node_evaluator nodes >>
                pure ()
            ) >>= \ ((), TS ts_decls ts_nominal_types ts_make_thunk_graphs ts_lambdas ts_evaluators) ->

            mapM stringify_ts_decl ts_decls >>= \ ts_decls ->
            mapM stringify_ts_nominal_type ts_nominal_types >>= \ ts_nominal_types ->
            mapM stringify_ts_make_thunk_graph ts_make_thunk_graphs >>= \ ts_make_thunk_graphs ->
            mapM stringify_ts_lambda ts_lambdas >>= \ ts_lambdas ->
            mapM stringify_ts_evaluator ts_evaluators >>= \ ts_evaluators ->

            pure (runtime_code <> Text.concat ts_decls <> Text.concat ts_nominal_types <> Text.concat ts_make_thunk_graphs <> Text.concat ts_lambdas <> Text.concat ts_evaluators)
        )
        (nominal_types, nodes, params)

define_decl :: IR.DeclKey -> Decl -> TSWriter ()
define_decl _ (IR.Decl'Module _) = pure ()
define_decl _ (IR.Decl'Type _) = pure ()

define_nominal_type :: IR.NominalTypeKey -> NominalType -> TSWriter ()
define_nominal_type key (IR.NominalType'Data name variants) = tell_nominal_type (TSNominalType'Data key name)
define_nominal_type _ (IR.NominalType'Synonym _ _) = pure ()

define_graph_node_evaluator :: IR.GraphNodeKey -> GraphNode -> TSWriter ()
define_graph_node_evaluator key (IR.GraphNode'Int ty i) = tell_evaluator $ TSEvaluator key ty [] ("return " <> show i <> ";\n")
define_graph_node_evaluator key (IR.GraphNode'Float ty (num :% denom)) = tell_evaluator $ TSEvaluator key ty [] ("return " <> show num <> " / " <> show denom <> ";\n")
define_graph_node_evaluator key (IR.GraphNode'Bool ty b) = tell_evaluator $ TSEvaluator key ty [] ("return " <> (if b then "true" else "false") <> ";\n")
define_graph_node_evaluator key (IR.GraphNode'Char ty c) = tell_evaluator $ TSEvaluator key ty [] ("return " <> show c <> ";\n")
define_graph_node_evaluator key (IR.GraphNode'String ty s) = tell_evaluator $ TSEvaluator key ty [] ("return " <> show s <> ";\n")
define_graph_node_evaluator key (IR.GraphNode'Tuple ty a b) =
    lift (node_type a) >>= \ a_ty ->
    lift (node_type b) >>= \ b_ty ->
    tell_evaluator $ TSEvaluator key ty [("a", a_ty), ("b", b_ty)] "return [this.a, this.b];\n"

define_graph_node_evaluator key (IR.GraphNode'Lambda ty param body) = -- TODO: annotate with captures
    lift (get_param param) >>= \ (IR.GraphParam param_ty) ->
    lift (node_type body) >>= \ body_type ->
    lift (get_included_nodes body) >>= \ (included_nodes, included_params) ->
    tell_make_thunk_graph (TSMakeThunkGraph body included_nodes included_params) >>
    tell_lambda (TSLambda key param_ty body_type body) >>
    tell_evaluator (TSEvaluator key ty [] ("return new " <> mangle_graph_node_as_lambda key <> "();\n")) -- TODO
    where
        get_included_nodes :: IR.GraphNodeKey -> IRReader (Set IR.GraphNodeKey, Set IR.GraphParamKey)
        get_included_nodes cur_node =
            runWriterT (runWriterT (get_included_nodes' cur_node)) >>= \ (((), nodes), params) ->
            pure (nodes, params)

        get_included_nodes' cur_node =
            -- TODO: prevent infinite recursion
            tell (Set.singleton cur_node) >>
            lift (lift (get_node cur_node)) >>= \case
                IR.GraphNode'Int _ _ -> pure ()
                IR.GraphNode'Float _ _ -> pure ()
                IR.GraphNode'Bool _ _ -> pure ()
                IR.GraphNode'Char _ _ -> pure ()
                IR.GraphNode'String _ _ -> pure ()
                IR.GraphNode'Tuple _ a b -> get_included_nodes' a >> get_included_nodes' b

                IR.GraphNode'Lambda _ _ _ -> pure () -- param of lambda is not referenced, and body is not turned into thunks here
                IR.GraphNode'Param _ param -> lift $ tell (Set.singleton param)

                IR.GraphNode'Call _ callee arg -> get_included_nodes' callee >> get_included_nodes' arg

                IR.GraphNode'TupleDestructure1 _ tup -> get_included_nodes' tup
                IR.GraphNode'TupleDestructure2 _ tup -> get_included_nodes' tup

                IR.GraphNode'Poison _ void -> absurd void

define_graph_node_evaluator _ (IR.GraphNode'Param _ _) = pure () -- params do not get lowered and instead are replaced by lambda calls

define_graph_node_evaluator key (IR.GraphNode'Call ty callee arg) =
    lift (node_type callee) >>= \ callee_type ->
    lift (node_type arg) >>= \ arg_type ->
    tell_evaluator $ TSEvaluator key ty [("callee", callee_type), ("arg", arg_type)] "return this.callee.get_value().call(this.arg).get_value();\n"

define_graph_node_evaluator key (IR.GraphNode'TupleDestructure1 ty tup) = lift (node_type tup) >>= \ tup_ty -> tell_evaluator $ TSEvaluator key ty [("tup", tup_ty)] "return this.tup.get_value()[0].get_value();\n"
define_graph_node_evaluator key (IR.GraphNode'TupleDestructure2 ty tup) = lift (node_type tup) >>= \ tup_ty -> tell_evaluator $ TSEvaluator key ty [("tup", tup_ty)] "return this.tup.get_value()[1].get_value();\n"

define_graph_node_evaluator _ (IR.GraphNode'Poison _ void) = absurd void

-- mangling {{{2
-- TODO: better mangling and unified mangling for everything
mangle_nominal_type :: IR.NominalTypeKey -> Text
mangle_nominal_type key = "NominalType" <> show (Arena.unmake_key key)

mangle_graph_node_as_lambda :: IR.GraphNodeKey -> Text
mangle_graph_node_as_lambda key = "Lambda" <> show (Arena.unmake_key key)

mangle_graph_node_as_make_thunk_graph :: IR.GraphNodeKey -> Text
mangle_graph_node_as_make_thunk_graph key = "make_thunk_graph" <> show (Arena.unmake_key key)

mangle_graph_node_as_evaluator :: IR.GraphNodeKey -> Text
mangle_graph_node_as_evaluator key = "Evaluator" <> show (Arena.unmake_key key)