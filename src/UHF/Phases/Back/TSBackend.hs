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
stringify_ts_adt (TSADT key ) =
    pure $
        "class " <> mangle_adt key <> " {\n"
            <> "}\n"

stringify_ts_make_thunk_graph :: TSMakeThunkGraph -> IRReader Text
stringify_ts_make_thunk_graph (TSMakeThunkGraph for included_bindings captures param) =
    sequence (stringify_param <$> param) >>= \ stringified_param ->
    sequence (map stringify_capture $ Set.toList captures) >>= \ stringified_captures ->

    concat <$> mapM stringify_binding_decl included_bindings >>= \ binding_decls ->
    concat <$> mapM stringify_binding_set_fields included_bindings >>= \ binding_set_fields ->
    -- TODO: dont use unmake_key anywhere (probably including outside of this module too)

    ts_return_type >>= \ ts_return_type ->

    pure ("function " <> mangle_make_thunk_graph_for for <> "(" <> Text.intercalate ", " (stringified_captures ++ Maybe.maybeToList stringified_param) <> "): " <> ts_return_type <> " {\n"
        <> Text.unlines (map ("    " <>) binding_decls)
        <> "\n"
        <> Text.unlines (map ("    " <>) binding_set_fields)
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
                    pure (mangle_binding_as_binding_var binding <> ": " <> ty)

        object_of_bindings = "{ " <> Text.intercalate ", " (map r included_bindings) <> " }"
            where
                r binding =
                    mangle_binding_as_binding_var binding <> ": " <> mangle_binding_as_thunk binding

        mangle_binding_as_local_evaluator :: ANFIR.BindingKey -> Text
        mangle_binding_as_local_evaluator key = "evaluator" <> show (Arena.unmake_key key)

        stringify_param param_key =
            get_param param_key >>= \ (ANFIR.Param param_ty) ->
            refer_type param_ty >>= \ ty_refer ->
            pure ("param_" <> show (Arena.unmake_key param_key) <> ": " <> ty_refer)

        stringify_capture bk =
            ANFIR.binding_type <$> get_binding bk >>= refer_type >>= \ ty_refer ->
            pure (mangle_binding_as_thunk bk <> ": " <> ty_refer)

        stringify_binding_decl binding_key =
            binding_type binding_key >>= refer_type >>= \ cur_binding_type ->
            let evaluator evaluator_name evaluator_args = "new " <> evaluator_name <> "(" <> evaluator_args <> ")"
                let_evaluator evaluator_type initializer = "let " <> mangle_binding_as_local_evaluator binding_key <> ": " <> evaluator_type <> " = " <> initializer <> ";"
                let_thunk :: Text -> Text
                let_thunk initializer = "let " <> mangle_binding_as_thunk binding_key <> ": " <> cur_binding_type <> " = " <> initializer <> ";"
                default_let_thunk = let_thunk ("new Thunk(" <> mangle_binding_as_local_evaluator binding_key <> ")")

            in ANFIR.get_initializer <$> get_binding binding_key >>= \case
                ANFIR.Expr'Identifier ty _ ->
                    refer_type_raw ty >>= \ ty ->
                    pure [let_evaluator ("PassthroughEvaluator<" <> ty <> ">") (evaluator "PassthroughEvaluator" "undefined"), default_let_thunk]
                ANFIR.Expr'Int _ i -> pure [let_evaluator "ConstEvaluator<Int>" (evaluator "ConstEvaluator" ("new Int(" <> show i <> ")")), default_let_thunk]
                ANFIR.Expr'Float _ (num :% denom) -> pure [let_evaluator "ConstEvaluator<Float>" (evaluator "ConstEvaluator" ("new Float(" <> show num <> " / " <> show denom <> ")")), default_let_thunk]
                ANFIR.Expr'Bool _ b -> pure [let_evaluator "ConstEvaluator<Bool>" (evaluator "ConstEvaluator" ("new Bool(" <> if b then "true" else "false" <> ")")), default_let_thunk]
                ANFIR.Expr'Char _ c -> pure [let_evaluator "ConstEvaluator<Char>" (evaluator "ConstEvaluator" ("new Char(" <> show c <> ")")), default_let_thunk]
                ANFIR.Expr'String _ s -> pure [let_evaluator "ConstEvaluator<UHFString>" (evaluator "ConstEvaluator" ("new UHFString(" <> show s <> ")")), default_let_thunk]
                ANFIR.Expr'Tuple _ a b ->
                        binding_type a >>= refer_type_raw >>= \ a_ty ->
                        binding_type b >>= refer_type_raw >>= \ b_ty ->
                        pure [let_evaluator ("TupleEvaluator<" <> a_ty <> ", " <> b_ty <> ">") (evaluator "TupleEvaluator" "undefined, undefined"), default_let_thunk]

                ANFIR.Expr'Lambda _ captures _ _ _ -> pure [let_evaluator ("ConstEvaluator<" <> mangle_binding_as_lambda binding_key <> ">") (evaluator "ConstEvaluator" ("new " <> mangle_binding_as_lambda binding_key <> "(" <> Text.intercalate ", " (map (const "undefined") (toList captures)) <> ")")), default_let_thunk] -- TODO: put captures here
                ANFIR.Expr'Param _ param_key -> pure [let_thunk $ "param_" <> show (Arena.unmake_key param_key)]

                ANFIR.Expr'Call ty _ arg ->
                    refer_type_raw ty >>= \ res_ty ->
                    binding_type arg >>= refer_type_raw >>= \ arg_ty ->
                    pure [let_evaluator ("CallEvaluator<" <> arg_ty <> ", " <> res_ty <> ">") (evaluator "CallEvaluator" "undefined, undefined"), default_let_thunk]

                ANFIR.Expr'Switch ty e _ ->
                    refer_type_raw ty >>= \ res_ty ->
                    binding_type e >>= refer_type_raw >>= \ e_ty ->
                    pure [let_evaluator ("SwitchEvaluator<" <> e_ty <> ", " <> res_ty <> ">") (evaluator "SwitchEvaluator" "undefined, undefined"), default_let_thunk]

                ANFIR.Expr'TupleDestructure1 ty _ -> refer_type_raw ty >>= \ ty -> pure [let_evaluator ("TupleDestructure1Evaluator<" <> ty <> ">") (evaluator "TupleDestructure1Evaluator" "undefined"), default_let_thunk]
                ANFIR.Expr'TupleDestructure2 ty _ -> refer_type_raw ty >>= \ ty -> pure [let_evaluator ("TupleDestructure2Evaluator<" <> ty <> ">") (evaluator "TupleDestructure2Evaluator" "undefined"), default_let_thunk]

                ANFIR.Expr'Poison _ void -> absurd void

        stringify_binding_set_fields binding_key =
            let set_field field other_binding = set_field_not_binding field (mangle_binding_as_thunk other_binding)
                set_field_not_binding field initializer = mangle_binding_as_local_evaluator binding_key <> "." <> field <> " = " <> initializer <> ";"
            in ANFIR.get_initializer <$> get_binding binding_key >>= \case
                ANFIR.Expr'Identifier _ i -> pure [set_field "other" i]
                ANFIR.Expr'Int _ _ -> pure []
                ANFIR.Expr'Float _ _ -> pure []
                ANFIR.Expr'Bool _ _ -> pure []
                ANFIR.Expr'Char _ _ -> pure []
                ANFIR.Expr'String _ _ -> pure []
                ANFIR.Expr'Tuple _ a b -> pure [set_field "a" a, set_field "b" b]

                ANFIR.Expr'Lambda _ captures _ _ _ -> pure $ map (\ capt -> set_field ("value." <> mangle_binding_as_capture capt) capt) (toList captures)
                ANFIR.Expr'Param _ _ -> pure []

                ANFIR.Expr'Switch _ e arms -> pure [set_field "testing" e, set_field_not_binding "arms" ("[" <> Text.intercalate ", " (map (\ (matcher, res) -> "[" <> convert_matcher matcher <> ", " <> mangle_binding_as_thunk res <> "]") arms) <> "]")] -- TODO: exhaustiveness check

                ANFIR.Expr'Call _ callee arg -> pure [set_field "callee" callee, set_field "arg" arg]

                ANFIR.Expr'TupleDestructure1 _ tup -> pure [set_field "tuple" tup]
                ANFIR.Expr'TupleDestructure2 _ tup -> pure [set_field "tuple" tup]

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
        <> "        return " <> mangle_make_thunk_graph_for (LambdaBody key) <> "(" <> Text.intercalate ", " (map (\ c -> "this." <> mangle_binding_as_capture c) (toList captures) ++ ["arg"]) <> ")." <> mangle_binding_as_binding_var body_key <> ";\n"
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
        <> Text.concat (map (\ (TSGlobalThunk k) -> "    " <> mangle_binding_as_thunk k <> " = " <> "globals." <> mangle_binding_as_binding_var k <> ";\n") thunks)
        <> "}\n")

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
lower :: DeclArena -> ADTArena -> TypeSynonymArena -> BindingArena -> ParamArena -> Text
lower decls adts type_synonyms bindings params =
    runReader
        (
            runWriterT (
                Arena.transform_with_keyM define_decl decls >> -- TODO: pass module instead of doing this
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

mangle_binding_as_binding_var :: ANFIR.BindingKey -> Text
mangle_binding_as_binding_var key = "binding_" <> show (Arena.unmake_key key)

mangle_binding_as_capture :: ANFIR.BindingKey -> Text
mangle_binding_as_capture key = "capture" <> show (Arena.unmake_key key)

mangle_binding_as_thunk :: ANFIR.BindingKey -> Text
mangle_binding_as_thunk key = "thunk" <> show (Arena.unmake_key key)

mangle_make_thunk_graph_for :: MakeThunkGraphFor -> Text
mangle_make_thunk_graph_for Globals = "make_global_thunk_graph"
mangle_make_thunk_graph_for (LambdaBody lambda_key) = "make_thunk_graph_for_" <> mangle_binding_as_lambda lambda_key -- TODO: put this in the lambda itself?
