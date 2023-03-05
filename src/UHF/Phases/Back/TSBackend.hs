module UHF.Phases.Back.TSBackend (lower) where

import UHF.Util.Prelude

import qualified Arena

import qualified Data.Text as Text
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.FileEmbed as FileEmbed

import qualified UHF.Data.IR.ANFIR as ANFIR
import qualified UHF.Data.IR.Type as Type
import UHF.Data.IR.Keys

type Decl = ANFIR.Decl
type DeclArena = Arena.Arena Decl DeclKey

type Type = Type.Type Void
type ADT = Type.ADT Type
type TypeSynonym = Type.TypeSynonym Type
type Binding = ANFIR.Binding Type Void
type Param = ANFIR.Param Type

type ADTArena = Arena.Arena ADT ADTKey
type TypeSynonymArena = Arena.Arena TypeSynonym Type.TypeSynonymKey
type BindingArena = Arena.Arena Binding ANFIR.BindingKey
type ParamArena = Arena.Arena Param ANFIR.ParamKey

-- TODO: refactor everything

type IRReader = Reader (ADTArena, TypeSynonymArena, BindingArena, ParamArena)

get_binding :: ANFIR.BindingKey -> IRReader Binding
get_binding k = reader (\ (_, _, a, _) -> Arena.get a k)
get_param :: ANFIR.ParamKey -> IRReader Param
get_param k = reader (\ (_, _, _, a) -> Arena.get a k)
get_adt :: ADTKey -> IRReader ADT
get_adt k = reader (\ (a, _, _, _) -> Arena.get a k)
get_type_synonym :: Type.TypeSynonymKey -> IRReader TypeSynonym
get_type_synonym k = reader (\ (_, a, _, _) -> Arena.get a k)

binding_type :: ANFIR.BindingKey -> IRReader Type
binding_type k = ANFIR.binding_type <$> get_binding k

-- TS things {{{1
runtime_code :: Text
runtime_code = $(FileEmbed.embedStringFile "data/ts_runtime.ts")

data TSDecl
data TSADT = TSADT ADTKey -- TODO: actually implement variants and things
data TSLambda = TSLambda ANFIR.BindingKey (Set ANFIR.BindingKey) Type Type ANFIR.BindingKey
data TSGlobalThunk = TSGlobalThunk ANFIR.BindingKey
-- TODO: dont use BoundValueKey Ord for order of captures in parameters of function
data TSMakeThunkGraph = TSMakeThunkGraph MakeThunkGraphFor [ANFIR.BindingKey] (Set ANFIR.BindingKey) (Maybe ANFIR.ParamKey) -- list of bindings is body, set of bindings is captures
data MakeThunkGraphFor = LambdaBody ANFIR.BindingKey | Globals
data TS = TS [TSDecl] [TSADT] [TSMakeThunkGraph] [TSLambda] [TSGlobalThunk]

instance Semigroup TS where
    (TS d1 n1 m1 l1 g1) <> (TS d2 n2 m2 l2 g2) = TS (d1 <> d2) (n1 <> n2) (m1 <> m2) (l1 <> l2) (g1 <> g2)
instance Monoid TS where
    mempty = TS mempty mempty mempty mempty mempty

type TSWriter = WriterT TS IRReader

tell_adt :: TSADT -> TSWriter ()
tell_adt adt = tell $ TS [] [adt] [] [] []
tell_make_thunk_graph :: TSMakeThunkGraph -> TSWriter ()
tell_make_thunk_graph mtg = tell $ TS [] [] [mtg] [] []
tell_lambda :: TSLambda -> TSWriter ()
tell_lambda lt = tell $ TS [] [] [] [lt] []
tell_global :: TSGlobalThunk -> TSWriter ()
tell_global gt = tell $ TS [] [] [] [] [gt]

stringify_ts_decl :: TSDecl -> IRReader Text
stringify_ts_decl = error "unreachable"

stringify_ts_adt :: TSADT -> IRReader Text
stringify_ts_adt (TSADT key) =
    pure $
        "class " <> mangle_adt key <> " {\n"
            <> "}\n"

stringify_ts_make_thunk_graph :: TSMakeThunkGraph -> IRReader Text
stringify_ts_make_thunk_graph (TSMakeThunkGraph for included_bindings captures param) =
    sequence (stringify_param <$> param) >>= \ stringified_param ->
    sequence (map stringify_capture $ Set.toList captures) >>= \ stringified_captures ->

    (unzip <$> mapM stringify_binding_decl included_bindings) >>= \ (binding_decls, binding_set_evaluators) ->
    -- TODO: dont use unmake_key anywhere (probably including outside of this module too)

    ts_return_type >>= \ ts_return_type ->

    pure ("function " <> mangle_make_thunk_graph_for for <> "(" <> Text.intercalate ", " (stringified_captures ++ Maybe.maybeToList stringified_param) <> "): " <> ts_return_type <> " {\n"
        <> Text.unlines (map ("    " <>) binding_decls)
        <> "\n"
        <> Text.unlines (map ("    " <>) (Maybe.catMaybes binding_set_evaluators))
        <> "\n"
        <> "    return " <> object_of_bindings <> ";\n"
        <> "}\n")
    where
        ts_return_type =
            mapM r included_bindings >>= \ fields ->
            pure ("{ " <> Text.intercalate "; " fields <> " }")
            where
                r binding =
                    binding_type binding >>= refer_type >>= \ ty ->
                    pure (mangle_binding_as_thunk binding <> ": " <> ty)

        object_of_bindings = "{ " <> Text.intercalate ", " (map mangle_binding_as_thunk included_bindings) <> " }"

        stringify_param param_key =
            get_param param_key >>= \ (ANFIR.Param param_ty) ->
            refer_type param_ty >>= \ ty_refer ->
            pure ("param: " <> ty_refer)

        stringify_capture bk =
            ANFIR.binding_type <$> get_binding bk >>= refer_type >>= \ ty_refer ->
            pure (mangle_binding_as_thunk bk <> ": " <> ty_refer)

        stringify_binding_decl binding_key =
            binding_type binding_key >>= refer_type >>= \ cur_binding_type ->
            let set_evaluator evaluator_name evaluator_args = mangle_binding_as_thunk binding_key <> ".evaluator = new " <> evaluator_name <> "(" <> evaluator_args <> ");"
                let_thunk initializer = "let " <> mangle_binding_as_thunk binding_key <> ": " <> cur_binding_type <> " = " <> initializer <> ";"
                default_let_thunk = let_thunk ("new Thunk(undefined)")

            in ANFIR.get_initializer <$> get_binding binding_key >>= \case
                ANFIR.Expr'Identifier _ i ->
                    pure (default_let_thunk, Just (set_evaluator "PassthroughEvaluator" (mangle_binding_as_thunk i)))
                ANFIR.Expr'Int _ i -> pure (default_let_thunk, Just (set_evaluator "ConstEvaluator" ("new Int(" <> show i <> ")")))
                ANFIR.Expr'Float _ (num :% denom) -> pure (default_let_thunk, Just (set_evaluator "ConstEvaluator" ("new Float(" <> show num <> " / " <> show denom <> ")")))
                ANFIR.Expr'Bool _ b -> pure (default_let_thunk, Just (set_evaluator "ConstEvaluator" ("new Bool(" <> if b then "true" else "false" <> ")")))
                ANFIR.Expr'Char _ c -> pure (default_let_thunk, Just (set_evaluator "ConstEvaluator" ("new Char(" <> show c <> ")")))
                ANFIR.Expr'String _ s -> pure (default_let_thunk, Just (set_evaluator "ConstEvaluator" ("new UHFString(" <> show s <> ")")))
                ANFIR.Expr'Tuple _ a b ->
                        pure (default_let_thunk, Just (set_evaluator "TupleEvaluator" (mangle_binding_as_thunk a <> ", " <> mangle_binding_as_thunk b)))

                ANFIR.Expr'Lambda _ captures _ _ _ -> pure (default_let_thunk, Just (set_evaluator "ConstEvaluator" ("new " <> mangle_binding_as_lambda binding_key <> "(" <> Text.intercalate ", " (map mangle_binding_as_thunk (toList captures)) <> ")")))
                ANFIR.Expr'Param _ _ -> pure (let_thunk "param", Nothing)

                ANFIR.Expr'Call _ callee arg -> pure (default_let_thunk, Just (set_evaluator "CallEvaluator" (mangle_binding_as_thunk callee <> ", " <> mangle_binding_as_thunk arg)))

                ANFIR.Expr'Switch _ test arms -> pure (default_let_thunk, Just (set_evaluator "SwitchEvaluator" (mangle_binding_as_thunk test <> ", " <> ("[" <> Text.intercalate ", " (map (\ (matcher, res) -> "[" <> convert_matcher matcher <> ", " <> mangle_binding_as_thunk res <> "]") arms) <> "]"))))

                ANFIR.Expr'TupleDestructure1 _ tup -> pure (default_let_thunk, Just (set_evaluator "TupleDestructure1Evaluator" (mangle_binding_as_thunk tup)))
                ANFIR.Expr'TupleDestructure2 _ tup -> pure (default_let_thunk, Just (set_evaluator "TupleDestructure2Evaluator" (mangle_binding_as_thunk tup)))

                ANFIR.Expr'Poison _ void -> absurd void

        convert_matcher (ANFIR.Switch'BoolLiteral b) = "new BoolLiteralMatcher(" <> if b then "true" else "false" <> ")"
        convert_matcher ANFIR.Switch'Tuple = "new TupleMatcher()"
        convert_matcher ANFIR.Switch'Default = "new DefaultMatcher()"

stringify_ts_lambda :: TSLambda -> IRReader Text
stringify_ts_lambda (TSLambda key captures arg result body_key) =
    refer_type_raw arg >>= \ arg_type_raw ->
    refer_type arg >>= \ arg_type ->
    refer_type_raw result >>= \ result_type_raw ->
    refer_type result >>= \ result_type ->

    Text.intercalate ", " <$>
        (mapM
            (\ c ->
                ANFIR.binding_type <$> get_binding c >>= refer_type >>= \ c_ty ->
                pure ("public " <> mangle_binding_as_capture c <> ": " <> c_ty))
            (toList captures)) >>= \ capture_constructor_params ->

    pure ("class " <> mangle_binding_as_lambda key <> " implements Lambda<" <> arg_type_raw <> ", " <> result_type_raw <> "> {\n"
        <> "    constructor(" <> capture_constructor_params  <> ") {}\n"
        <> "    call(arg: " <> arg_type <> "): " <> result_type <> " {\n"
        <> "        return " <> mangle_make_thunk_graph_for (LambdaBody key) <> "(" <> Text.intercalate ", " (map (\ c -> "this." <> mangle_binding_as_capture c) (toList captures) ++ ["arg"]) <> ")." <> mangle_binding_as_thunk body_key <> ";\n"
        <> "    }\n"
        <> "}\n")

stringify_ts_global_thunk :: TSGlobalThunk -> IRReader Text
stringify_ts_global_thunk (TSGlobalThunk key) =
    binding_type key >>= refer_type >>= \ ty ->
    pure ("let " <> mangle_binding_as_thunk key <> ": " <> ty <> ";\n")

initialize_global_thunks :: [TSGlobalThunk] -> IRReader Text
initialize_global_thunks thunks =
    pure ("function initialize_global_thunks() {\n"
        <> "    let globals = make_global_thunk_graph();\n"
        <> Text.concat (map (\ (TSGlobalThunk k) -> "    " <> mangle_binding_as_thunk k <> " = " <> "globals." <> mangle_binding_as_thunk k <> ";\n") thunks)
        <> "}\n"
        <> "initialize_global_thunks();\n")

-- referring to types {{{2
refer_type_raw :: Type.Type Void -> IRReader Text
refer_type_raw (Type.Type'ADT ak) = pure $ mangle_adt ak

refer_type_raw (Type.Type'Synonym sk) =
    get_type_synonym sk >>= \ (Type.TypeSynonym _ expansion) -> refer_type expansion

refer_type_raw Type.Type'Int = pure "Int"
refer_type_raw Type.Type'Float = pure "Float"
refer_type_raw Type.Type'Char = pure "Char"
refer_type_raw Type.Type'String = pure "UHFString"
refer_type_raw Type.Type'Bool = pure "Bool"
refer_type_raw (Type.Type'Function a r) = refer_type_raw a >>= \ a -> refer_type_raw r >>= \ r -> pure ("Lambda<" <> a <> ", " <> r <> ">")
refer_type_raw (Type.Type'Tuple a b) = refer_type_raw a >>= \ a -> refer_type_raw b >>= \ b -> pure ("Tuple<" <> a <> ", " <> b <> ">")
refer_type_raw (Type.Type'Variable void) = absurd void

refer_type :: Type.Type Void -> IRReader Text
refer_type ty = refer_type_raw ty >>= \ ty -> pure ("Thunk<" <> ty <> ">")

-- lowering {{{1
-- TODO: take ANFIR
lower :: DeclArena -> ADTArena -> TypeSynonymArena -> BindingArena -> ParamArena -> DeclKey -> Text
lower decls adts type_synonyms bindings params mod =
    runReader
        (
            runWriterT (
                define_decl mod (Arena.get decls mod) >>
                Arena.transform_with_keyM define_lambda_type bindings >>
                pure ()
            ) >>= \ ((), TS ts_decls ts_adts ts_make_thunk_graphs ts_lambdas ts_global_thunks) ->

            mapM stringify_ts_decl ts_decls >>= \ ts_decls ->
            mapM stringify_ts_adt ts_adts >>= \ ts_adts ->
            mapM stringify_ts_make_thunk_graph ts_make_thunk_graphs >>= \ ts_make_thunk_graphs ->
            mapM stringify_ts_lambda ts_lambdas >>= \ ts_lambdas ->
            initialize_global_thunks ts_global_thunks >>= \ initialize_global_thunks ->
            mapM stringify_ts_global_thunk ts_global_thunks >>= \ ts_global_thunks ->

            pure (runtime_code <> Text.concat ts_decls <> Text.concat ts_adts <> Text.concat ts_global_thunks <> Text.concat ts_make_thunk_graphs <> Text.concat ts_lambdas <> initialize_global_thunks)
        )
        (adts, type_synonyms, bindings, params)

define_decl :: DeclKey -> Decl -> TSWriter ()
define_decl _ (ANFIR.Decl'Module global_bindings adts _) =
    mapM_ (tell_adt . TSADT) adts >>
    mapM (tell_global . TSGlobalThunk) global_bindings >>
    tell_make_thunk_graph (TSMakeThunkGraph Globals global_bindings Set.empty Nothing) -- global thunk graph does not have any params >>
define_decl _ (ANFIR.Decl'Type _) = pure ()

define_lambda_type :: ANFIR.BindingKey -> Binding -> TSWriter ()
define_lambda_type key (ANFIR.Binding (ANFIR.Expr'Lambda _ captures param body_included_bindings body)) =
    lift (get_param param) >>= \ (ANFIR.Param param_ty) ->
    lift (binding_type body) >>= \ body_type ->
    tell_make_thunk_graph (TSMakeThunkGraph (LambdaBody key) body_included_bindings captures (Just param)) >>
    tell_lambda (TSLambda key captures param_ty body_type body)

define_lambda_type _ _ = pure ()

-- mangling {{{2
-- TODO: better mangling and unified mangling for everything
mangle_adt :: ADTKey -> Text
mangle_adt key = "ADT" <> show (Arena.unmake_key key)

mangle_binding_as_lambda :: ANFIR.BindingKey -> Text
mangle_binding_as_lambda key = "Lambda" <> show (Arena.unmake_key key)

mangle_binding_as_capture :: ANFIR.BindingKey -> Text
mangle_binding_as_capture key = "capture" <> show (Arena.unmake_key key)

mangle_binding_as_thunk :: ANFIR.BindingKey -> Text
mangle_binding_as_thunk key = "thunk" <> show (Arena.unmake_key key)

mangle_make_thunk_graph_for :: MakeThunkGraphFor -> Text
mangle_make_thunk_graph_for Globals = "make_global_thunk_graph"
mangle_make_thunk_graph_for (LambdaBody lambda_key) = "make_thunk_graph_for_" <> mangle_binding_as_lambda lambda_key
