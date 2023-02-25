module UHF.TSBackend (lower) where

import UHF.Util.Prelude

import qualified Arena

import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Data.FileEmbed as FileEmbed

import UHF.IO.Located (Located)

import qualified UHF.HIR as HIR
import qualified UHF.ANFIR as ANFIR

-- TODO: same todo as in ToDot and RemovePoison
type Decl = HIR.Decl (Located (Maybe HIR.BoundValueKey)) (Maybe (HIR.Type Void)) (Maybe (HIR.Type Void)) Void
type DeclArena = Arena.Arena Decl HIR.DeclKey

type Type = HIR.Type Void
type NominalType = HIR.NominalType Type
type GraphNode = ANFIR.Node Type Void
type GraphParam = ANFIR.Param Type

type NominalTypeArena = Arena.Arena NominalType HIR.NominalTypeKey
type GraphNodeArena = Arena.Arena GraphNode ANFIR.NodeKey
type GraphParamArena = Arena.Arena GraphParam ANFIR.ParamKey

-- TODO: refactor everything
-- TODO: this actually does not work correctly like at all

type IRReader = Reader (NominalTypeArena, GraphNodeArena, GraphParamArena)

get_node :: ANFIR.NodeKey -> IRReader GraphNode
get_node k = reader (\ (_, a, _) -> Arena.get a k)
get_param :: ANFIR.ParamKey -> IRReader GraphParam
get_param k = reader (\ (_, _, a) -> Arena.get a k)
get_nominal_type :: HIR.NominalTypeKey -> IRReader NominalType
get_nominal_type k = reader (\ (a, _, _) -> Arena.get a k)

node_type :: ANFIR.NodeKey -> IRReader Type
node_type k = ANFIR.node_type <$> get_node k

-- TS things {{{1
runtime_code :: Text
runtime_code = $(FileEmbed.embedStringFile "data/ts_runtime.ts")

data TSDecl
data TSNominalType = TSNominalType'Data HIR.NominalTypeKey Text -- TODO: actually implement variants and things
data TSLambda = TSLambda ANFIR.NodeKey Type Type ANFIR.NodeKey -- TODO: captures
data MakeThunkGraphFor = LambdaBody ANFIR.NodeKey | Globals
data TSMakeThunkGraph = TSMakeThunkGraph MakeThunkGraphFor (Set ANFIR.NodeKey) (Set ANFIR.ParamKey)
data TS = TS [TSDecl] [TSNominalType] [TSMakeThunkGraph] [TSLambda]

instance Semigroup TS where
    (TS d1 n1 m1 l1) <> (TS d2 n2 m2 l2) = TS (d1 <> d2) (n1 <> n2) (m1 <> m2) (l1 <> l2)
instance Monoid TS where
    mempty = TS mempty mempty mempty mempty

type TSWriter = WriterT TS IRReader

tell_nominal_type :: TSNominalType -> TSWriter ()
tell_nominal_type nt = tell $ TS [] [nt] [] []
tell_make_thunk_graph :: TSMakeThunkGraph -> TSWriter ()
tell_make_thunk_graph mtg = tell $ TS [] [] [mtg] []
tell_lambda :: TSLambda -> TSWriter ()
tell_lambda lt = tell $ TS [] [] [] [lt]

stringify_ts_decl :: TSDecl -> IRReader Text
stringify_ts_decl = error "unreachable"

stringify_ts_nominal_type :: TSNominalType -> IRReader Text
stringify_ts_nominal_type (TSNominalType'Data key name) =
    pure $
        "// data " <> name <> "\n"
            <> "class " <> mangle_nominal_type key <> " {\n"
            <> "}\n"

stringify_ts_make_thunk_graph :: TSMakeThunkGraph -> IRReader Text
stringify_ts_make_thunk_graph (TSMakeThunkGraph for included_nodes included_params) =
    mapM stringify_param (Set.toList included_params) >>= \ params_stringified ->

    concat <$> mapM stringify_node_decl (Set.toList included_nodes) >>= \ node_decls ->
    concat <$> mapM stringify_node_set_fields (Set.toList included_nodes) >>= \ node_set_fields ->
    -- TODO: dont use GraphParamKey Ord instance to decide parameter order (not deterministic across compiles)
    -- also TODO: dont use unmake_key anywhere (probably including outside of this module too)

    ts_return_type >>= \ ts_return_type ->

    pure ("function " <> mangle_make_thunk_graph_for for <> "(" <> Text.intercalate ", " params_stringified <> "): " <> ts_return_type <> " {\n" -- TODO: captures
        <> Text.unlines (map ("    " <>) node_decls)
        <> "\n"
        <> Text.unlines (map ("    " <>) node_set_fields)
        <> "\n"
        <> "    return " <> object_of_nodes <> ";\n"
        <> "}\n")
    where
        ts_return_type =
            mapM r (Set.toList included_nodes) >>= \ tys ->
            pure ("{ " <> Text.intercalate "; " tys <> " }")
            where
                r node =
                    node_type node >>= refer_type >>= \ ty ->
                    pure (mangle_graph_node_as_node_var node <> ": " <> ty)

        object_of_nodes = "{ " <> Text.intercalate ", " (map r (Set.toList included_nodes)) <> " }"
            where
                r node =
                    mangle_graph_node_as_node_var node <> ": " <> mangle_graph_node_as_local_thunk node

        mangle_graph_node_as_local_thunk, mangle_graph_node_as_local_evaluator :: ANFIR.NodeKey -> Text
        mangle_graph_node_as_local_thunk key = "thunk" <> show (Arena.unmake_key key)
        mangle_graph_node_as_local_evaluator key = "evaluator" <> show (Arena.unmake_key key)

        stringify_param param_key =
            get_param param_key >>= \ (ANFIR.Param param_ty) ->
            refer_type param_ty >>= \ ty_refer ->
            pure ("param_" <> show (Arena.unmake_key param_key) <> ": " <> ty_refer)

        stringify_node_decl node_key =
            node_type node_key >>= refer_type >>= \ cur_node_type ->
            let evaluator evaluator_name evaluator_args = "new " <> evaluator_name <> "(" <> evaluator_args <> ")"
                let_evaluator evaluator_type initializer = "let " <> mangle_graph_node_as_local_evaluator node_key <> ": " <> evaluator_type <> " = " <> initializer <> ";"
                let_thunk :: Text -> Text
                let_thunk initializer = "let " <> mangle_graph_node_as_local_thunk node_key <> ": " <> cur_node_type <> " = " <> initializer <> ";"
                default_let_thunk = let_thunk ("new Thunk(" <> mangle_graph_node_as_local_evaluator node_key <> ")")

            in get_node node_key >>= \case
                ANFIR.Node'Int _ i -> pure [let_evaluator "ConstEvaluator<number>" (evaluator "ConstEvaluator" (show i)), default_let_thunk]
                ANFIR.Node'Float _ (num :% denom) -> pure [let_evaluator "ConstEvaluator<number>" (evaluator "ConstEvaluator" (show num <> " / " <> show denom)), default_let_thunk]
                ANFIR.Node'Bool _ b -> pure [let_evaluator "ConstEvaluator<bool>" (evaluator "ConstEvaluator" (if b then "true" else "false")), default_let_thunk]
                ANFIR.Node'Char _ c -> pure [let_evaluator "ConstEvaluator<char>" (evaluator "ConstEvaluator" (show c)), default_let_thunk]
                ANFIR.Node'String _ s -> pure [let_evaluator "ConstEvaluator<string>" (evaluator "ConstEvaluator" (show s)), default_let_thunk]
                ANFIR.Node'Tuple _ a b ->
                        node_type a >>= refer_type_raw >>= \ a_ty ->
                        node_type b >>= refer_type_raw >>= \ b_ty ->
                        pure [let_evaluator ("TupleEvaluator<" <> a_ty <> ", " <> b_ty <> ">") (evaluator "TupleEvaluator" "undefined, undefined"), default_let_thunk]

                ANFIR.Node'Lambda _ _ _ -> pure [let_evaluator ("ConstEvaluator<" <> mangle_graph_node_as_lambda node_key <> ">") (evaluator "ConstEvaluator" ("new " <> mangle_graph_node_as_lambda node_key <> "()")), default_let_thunk] -- TODO: put captures here
                ANFIR.Node'Param _ param_key -> pure [let_thunk $ "param_" <> show (Arena.unmake_key param_key)]

                ANFIR.Node'Call ty _ arg ->
                    refer_type_raw ty >>= \ res_ty ->
                    node_type arg >>= refer_type_raw >>= \ arg_ty ->
                    pure [let_evaluator ("CallEvaluator<" <> arg_ty <> ", " <> res_ty <> ">") (evaluator "CallEvaluator" "undefined, undefined"), default_let_thunk]

                ANFIR.Node'TupleDestructure1 _ tup -> node_type tup >>= refer_type_raw >>= \ tup_ty -> pure [let_evaluator ("TupleDestructure1Evaluator<" <> tup_ty <> ">") (evaluator "TupleDestructure1Evaluator" "undefined"), default_let_thunk] -- TODO: fix this becuase the type parameters should actually be the elements of the tuple
                ANFIR.Node'TupleDestructure2 _ tup -> node_type tup >>= refer_type_raw >>= \ tup_ty -> pure [let_evaluator ("TupleDestructure1Evaluator<" <> tup_ty <> ">") (evaluator "TupleDestructure2Evaluator" "undefined"), default_let_thunk] -- TODO: same as above

                ANFIR.Node'Poison _ void -> absurd void

        stringify_node_set_fields node_key =
            let set_field field other_node = mangle_graph_node_as_local_evaluator node_key <> "." <> field <> " = " <> mangle_graph_node_as_local_thunk other_node <> ";"
            in get_node node_key >>= \case
                ANFIR.Node'Int _ _ -> pure []
                ANFIR.Node'Float _ _ -> pure []
                ANFIR.Node'Bool _ _ -> pure []
                ANFIR.Node'Char _ _ -> pure []
                ANFIR.Node'String _ _ -> pure []
                ANFIR.Node'Tuple _ a b -> pure [set_field "a" a, set_field "b" b]

                ANFIR.Node'Lambda _ _ _ -> pure []
                ANFIR.Node'Param _ _ -> pure []

                ANFIR.Node'Call _ callee arg -> pure [set_field "callee" callee, set_field "arg" arg]

                ANFIR.Node'TupleDestructure1 _ tup -> pure [set_field "tuple" tup]
                ANFIR.Node'TupleDestructure2 _ tup -> pure [set_field "tuple" tup]

                ANFIR.Node'Poison _ void -> absurd void

stringify_ts_lambda :: TSLambda -> IRReader Text
stringify_ts_lambda (TSLambda key arg result body_key) =
    refer_type arg >>= \ arg_type ->
    refer_type result >>= \ result_type ->
    pure ("class " <> mangle_graph_node_as_lambda key <> " implements Lambda<" <> arg_type <> ", " <> result_type <> "> {\n"
        <> "    call(arg: " <> arg_type <> "): " <> result_type <> " {\n"
        <> "        return " <> mangle_make_thunk_graph_for (LambdaBody key) <> "()." <> mangle_graph_node_as_node_var body_key <> ";\n" -- TODO: captures and params
        <> "    }\n"
        <> "}\n")

-- referring to types {{{2
refer_type_raw :: HIR.Type Void -> IRReader Text
refer_type_raw (HIR.Type'Nominal ntk) =
    get_nominal_type ntk >>= \case
        HIR.NominalType'Data _ _ -> pure $ mangle_nominal_type ntk
        HIR.NominalType'Synonym _ expansion -> refer_type expansion

refer_type_raw HIR.Type'Int = pure "number"
refer_type_raw HIR.Type'Float = pure "number"
refer_type_raw HIR.Type'Char = pure "char"
refer_type_raw HIR.Type'String = pure "string"
refer_type_raw HIR.Type'Bool = pure "bool"
refer_type_raw (HIR.Type'Function a r) = refer_type a >>= \ a -> refer_type r >>= \ r -> pure ("Lambda<" <> a <> ", " <> r <> ">")
refer_type_raw (HIR.Type'Tuple a b) = refer_type a >>= \ a -> refer_type b >>= \ b -> pure ("[" <> a <> ", " <> b <> "]")
refer_type_raw (HIR.Type'Variable void) = absurd void

refer_type :: HIR.Type Void -> IRReader Text
refer_type ty = refer_type_raw ty >>= \ ty -> pure ("Thunk<" <> ty <> ">")

-- lowering {{{1
lower :: DeclArena -> NominalTypeArena -> GraphNodeArena -> GraphParamArena -> Text
lower decls nominal_types nodes params =
    runReader
        (
            runWriterT (
                Arena.transform_with_keyM define_decl decls >>
                Arena.transform_with_keyM define_nominal_type nominal_types >>
                Arena.transform_with_keyM define_lambda_type nodes >>
                -- TODO: probably move collect involved nodes outside of define_lambda_type, also have to store global nodes in main module, tell_make_thunk_graph (TSMakeThunkGraph Globals global_nodes Set.empty) -- global thunk graph does not have any params
                pure ()
            ) >>= \ ((), TS ts_decls ts_nominal_types ts_make_thunk_graphs ts_lambdas) ->

            mapM stringify_ts_decl ts_decls >>= \ ts_decls ->
            mapM stringify_ts_nominal_type ts_nominal_types >>= \ ts_nominal_types ->
            mapM stringify_ts_make_thunk_graph ts_make_thunk_graphs >>= \ ts_make_thunk_graphs ->
            mapM stringify_ts_lambda ts_lambdas >>= \ ts_lambdas ->

            pure (runtime_code <> Text.concat ts_decls <> Text.concat ts_nominal_types <> Text.concat ts_make_thunk_graphs <> Text.concat ts_lambdas)
        )
        (nominal_types, nodes, params)

define_decl :: HIR.DeclKey -> Decl -> TSWriter ()
define_decl _ (HIR.Decl'Module _ _) = pure ()
define_decl _ (HIR.Decl'Type _) = pure ()

define_nominal_type :: HIR.NominalTypeKey -> NominalType -> TSWriter ()
define_nominal_type key (HIR.NominalType'Data name variants) = tell_nominal_type (TSNominalType'Data key name)
define_nominal_type _ (HIR.NominalType'Synonym _ _) = pure ()

define_lambda_type :: ANFIR.NodeKey -> GraphNode -> TSWriter ()
define_lambda_type key (ANFIR.Node'Lambda _ param body) = -- TODO: annotate with captures
    lift (get_param param) >>= \ (ANFIR.Param param_ty) ->
    lift (node_type body) >>= \ body_type ->
    lift (get_included_nodes body) >>= \ (included_nodes, included_params) ->
    tell_make_thunk_graph (TSMakeThunkGraph (LambdaBody key) included_nodes included_params) >>
    tell_lambda (TSLambda key param_ty body_type body)
    where
        get_included_nodes :: ANFIR.NodeKey -> IRReader (Set ANFIR.NodeKey, Set ANFIR.ParamKey)
        get_included_nodes cur_node =
            runWriterT (runWriterT (get_included_nodes' cur_node)) >>= \ (((), nodes), params) ->
            pure (nodes, params)

        get_included_nodes' cur_node =
            -- TODO: prevent infinite recursion
            tell (Set.singleton cur_node) >>
            lift (lift (get_node cur_node)) >>= \case
                ANFIR.Node'Int _ _ -> pure ()
                ANFIR.Node'Float _ _ -> pure ()
                ANFIR.Node'Bool _ _ -> pure ()
                ANFIR.Node'Char _ _ -> pure ()
                ANFIR.Node'String _ _ -> pure ()
                ANFIR.Node'Tuple _ a b -> get_included_nodes' a >> get_included_nodes' b

                ANFIR.Node'Lambda _ _ _ -> pure () -- param of lambda is not referenced, and body is not turned into thunks here
                ANFIR.Node'Param _ param -> lift $ tell (Set.singleton param)

                ANFIR.Node'Call _ callee arg -> get_included_nodes' callee >> get_included_nodes' arg

                ANFIR.Node'TupleDestructure1 _ tup -> get_included_nodes' tup
                ANFIR.Node'TupleDestructure2 _ tup -> get_included_nodes' tup

                ANFIR.Node'Poison _ void -> absurd void

define_lambda_type _ _ = pure ()

-- mangling {{{2
-- TODO: better mangling and unified mangling for everything
mangle_nominal_type :: HIR.NominalTypeKey -> Text
mangle_nominal_type key = "NominalType" <> show (Arena.unmake_key key)

mangle_graph_node_as_lambda :: ANFIR.NodeKey -> Text
mangle_graph_node_as_lambda key = "Lambda" <> show (Arena.unmake_key key)

mangle_graph_node_as_node_var :: ANFIR.NodeKey -> Text
mangle_graph_node_as_node_var key = "node_" <> show (Arena.unmake_key key)

mangle_make_thunk_graph_for :: MakeThunkGraphFor -> Text
mangle_make_thunk_graph_for Globals = "make_global_thunk_graph"
mangle_make_thunk_graph_for (LambdaBody lambda_key) = "make_thunk_graph_for_" <> mangle_graph_node_as_lambda lambda_key -- TODO: put this in the lambda itself?
