module UHF.Phases.Back.TSBackend (lower) where

import UHF.Util.Prelude

import qualified Arena
import qualified Unique

import qualified Data.Text as Text
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.FileEmbed as FileEmbed

import qualified UHF.Data.IR.ANFIR as ANFIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.ID as ID

type CaptureList = Set.Set ANFIR.BindingKey

type Decl = ANFIR.Decl CaptureList

type Type = Type.Type Void
type ADT = Type.ADT Type
type TypeSynonym = Type.TypeSynonym Type
type Binding = ANFIR.Binding CaptureList Type Void
type BindingGroup = ANFIR.BindingGroup CaptureList
type Param = ANFIR.Param Type

type ANFIR = ANFIR.ANFIR CaptureList Type Void

type ADTArena = Arena.Arena ADT Type.ADTKey
type TypeSynonymArena = Arena.Arena TypeSynonym Type.TypeSynonymKey
type BindingArena = Arena.Arena Binding ANFIR.BindingKey
type ParamArena = Arena.Arena Param ANFIR.ParamKey

-- TODO: refactor everything

type IRReader = Reader (ADTArena, TypeSynonymArena, BindingArena, ParamArena)

get_binding :: ANFIR.BindingKey -> IRReader Binding
get_binding k = reader $ \ (_, _, a, _) -> Arena.get a k
get_param :: ANFIR.ParamKey -> IRReader Param
get_param k = reader $ \ (_, _, _, a) -> Arena.get a k
get_adt :: Type.ADTKey -> IRReader ADT
get_adt k = reader $ \ (a, _, _, _) -> Arena.get a k
get_adt_arena :: IRReader ADTArena
get_adt_arena = reader $ \ (a, _, _, _) -> a
get_type_synonym :: Type.TypeSynonymKey -> IRReader TypeSynonym
get_type_synonym k = reader $ \ (_, a, _, _) -> Arena.get a k

binding_type :: ANFIR.BindingKey -> IRReader Type
binding_type k = ANFIR.binding_type <$> get_binding k

-- TS things {{{1
runtime_code :: Text
runtime_code = $(FileEmbed.embedStringFile "data/ts_runtime.ts")

data TSDecl
newtype TSADT = TSADT Type.ADTKey
data TSLambda = TSLambda ANFIR.BindingKey BindingGroup Type Type ANFIR.BindingKey
newtype TSGlobalThunk = TSGlobalThunk ANFIR.BindingKey
-- TODO: dont use BoundValueKey Ord for order of captures in parameters of function
data TSMakeThunkGraph = TSMakeThunkGraph BindingGroup (Maybe ANFIR.ParamKey) -- list of bindings is body, set of bindings is captures
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
    mangle_adt key >>= \ mangled ->
    data_type >>= \ data_type ->
    pure $
        "class " <> mangled <> " {\n"
            <> "    type: \"" <> mangled <> "\";\n"
            <> "    constructor(public data: " <> data_type <> ") {}\n"
            <> "}\n"
    where
        data_type =
            get_adt key >>= \ (Type.ADT _ _ _ variants) ->
            if null variants
                then pure "never"
                else mapM
                        (\ variant ->
                            let name_field = ("discriminant", "\"" <> Type.variant_name variant <> "\"")
                            in (case variant of
                                Type.ADTVariant'Named _ fields -> zipWithM (\ i (_, f) -> ("_" <> show (i :: Int),) <$> refer_type f) [0..] fields
                                Type.ADTVariant'Anon _ fields -> zipWithM (\ i f -> ("_" <> show (i :: Int),) <$> refer_type f) [0..] fields) >>= \ fields ->
                            pure ("{" <> Text.intercalate "; " (map (\ (n, t) -> n <> ": " <> t) (name_field : fields)) <> "}"))
                        variants >>= \ variant_types ->
                    pure (Text.intercalate " | " variant_types)

stringify_ts_make_thunk_graph :: TSMakeThunkGraph -> IRReader Text
stringify_ts_make_thunk_graph (TSMakeThunkGraph (ANFIR.BindingGroup unique captures included_bindings) param) =
    mapM stringify_param param >>= \ stringified_param ->
    mapM stringify_capture (Set.toList captures) >>= \ stringified_captures ->

    unzip <$> mapM stringify_binding_decl included_bindings >>= \ (binding_decls, binding_set_evaluators) ->
    mangle_make_thunk_group unique >>= \ fn_name ->
    ts_return_type >>= \ ts_return_type ->
    object_of_bindings >>= \ object_of_bindings ->

    pure ("function " <> fn_name <> "(" <> Text.intercalate ", " (stringified_captures ++ Maybe.maybeToList stringified_param) <> "): " <> ts_return_type <> " {\n"
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
                    mangle_binding_as_thunk binding >>= \ mangled ->
                    pure (mangled <> ": " <> ty)

        object_of_bindings =
            Text.intercalate ", " <$> mapM mangle_binding_as_thunk included_bindings >>= \ contents ->
            pure ("{ " <> contents <> " }")

        stringify_param param_key =
            get_param param_key >>= \ (ANFIR.Param _ param_ty) ->
            refer_type param_ty >>= \ ty_refer ->
            pure ("param: " <> ty_refer)

        stringify_capture bk =
            ANFIR.binding_type <$> get_binding bk >>= refer_type >>= \ ty_refer ->
            mangle_binding_as_thunk bk >>= \ mangled ->
            pure (mangled <> ": " <> ty_refer)

        stringify_binding_decl binding_key =
            binding_type binding_key >>= refer_type >>= \ cur_binding_type ->
            mangle_binding_as_thunk binding_key >>= \ binding_as_thunk ->
            let set_evaluator evaluator_name evaluator_args = binding_as_thunk <> ".evaluator = new " <> evaluator_name <> "(" <> evaluator_args <> ");"
                let_thunk initializer = "let " <> binding_as_thunk <> ": " <> cur_binding_type <> " = " <> initializer <> ";"
                default_let_thunk = let_thunk "new Thunk(undefined)"

            in ANFIR.binding_initializer <$> get_binding binding_key >>= \case
                ANFIR.Expr'Refer _ _ i ->
                    mangle_binding_as_thunk i >>= \ i_mangled ->
                    pure (default_let_thunk, Just (set_evaluator "PassthroughEvaluator" i_mangled))
                ANFIR.Expr'Int _ _ i -> pure (default_let_thunk, Just (set_evaluator "ConstEvaluator" ("new Int(" <> show i <> ")")))
                ANFIR.Expr'Float _ _ (num :% denom) -> pure (default_let_thunk, Just (set_evaluator "ConstEvaluator" ("new Float(" <> show num <> " / " <> show denom <> ")")))
                ANFIR.Expr'Bool _ _ b -> pure (default_let_thunk, Just (set_evaluator "ConstEvaluator" ("new Bool(" <> (if b then "true" else "false") <> ")")))
                ANFIR.Expr'Char _ _ c -> pure (default_let_thunk, Just (set_evaluator "ConstEvaluator" ("new Char(" <> show c <> ")")))
                ANFIR.Expr'String _ _ s -> pure (default_let_thunk, Just (set_evaluator "ConstEvaluator" ("new UHFString(" <> show s <> ")")))
                ANFIR.Expr'Tuple _ _ a b ->
                    mangle_binding_as_thunk a >>= \ a_mangled ->
                    mangle_binding_as_thunk b >>= \ b_mangled ->
                    pure (default_let_thunk, Just (set_evaluator "TupleEvaluator" (a_mangled <> ", " <> b_mangled)))

                ANFIR.Expr'Lambda _ _ _ group _ ->
                    let lambda_captures = ANFIR.binding_group_captures group
                    in mangle_binding_as_lambda binding_key >>= \ lambda ->
                    mapM mangle_binding_as_thunk (toList lambda_captures) >>= \ lambda_captures_mangled ->
                    pure (default_let_thunk, Just (set_evaluator "ConstEvaluator" ("new " <> lambda <> "(" <> Text.intercalate ", " lambda_captures_mangled <> ")")))
                ANFIR.Expr'Param _ _ _ -> pure (let_thunk "param", Nothing)

                ANFIR.Expr'Call _ _ callee arg ->
                    mangle_binding_as_thunk callee >>= \ callee_mangled ->
                    mangle_binding_as_thunk arg >>= \ arg_mangled ->
                    pure (default_let_thunk, Just (set_evaluator "CallEvaluator" (callee_mangled <> ", " <> arg_mangled)))

                ANFIR.Expr'Switch _ _ test arms ->
                    mapM (\ (matcher, group, res) -> -- TODO: lower binding group
                        mangle_make_thunk_group (ANFIR.binding_group_unique group) >>= \ make_group ->
                        mangle_binding_as_thunk res >>= \ res' ->
                        pure ("[" <> convert_matcher matcher <> ", " <> make_group <> "()." <> res' <> "]")) arms >>= \ arms' -> -- TODO: pass captures
                    mangle_binding_as_thunk test >>= \ test_mangled ->
                    pure (default_let_thunk, Just (set_evaluator "SwitchEvaluator" (test_mangled <> ", " <> ("[" <> Text.intercalate ", " arms' <> "]"))))

                ANFIR.Expr'Seq _ _ a b ->
                    mangle_binding_as_thunk a >>= \ a_mangled ->
                    mangle_binding_as_thunk b >>= \ b_mangled ->
                    pure (default_let_thunk, Just (set_evaluator "SeqEvaluator" (a_mangled <> ", " <> b_mangled)))

                ANFIR.Expr'TupleDestructure1 _ _ tup ->
                    mangle_binding_as_thunk tup >>= \ tup_mangled ->
                    pure (default_let_thunk, Just (set_evaluator "TupleDestructure1Evaluator" tup_mangled))
                ANFIR.Expr'TupleDestructure2 _ _ tup ->
                    mangle_binding_as_thunk tup >>= \ tup_mangled ->
                    pure (default_let_thunk, Just (set_evaluator "TupleDestructure2Evaluator" tup_mangled))

                -- foralls and type applications get erased, TODO: explain this better and also reconsider if this is actually correct
                -- TODO: lower binding group
                ANFIR.Expr'Forall _ _ _ bg e ->
                    mangle_make_thunk_group (ANFIR.binding_group_unique bg) >>= \ make_bg ->
                    mangle_binding_as_thunk e >>= \ e ->
                    pure (let_thunk $ make_bg <> "()." <> e, Nothing) -- TODO: pass captures
                ANFIR.Expr'TypeApply _ _ e _ ->
                    mangle_binding_as_thunk e >>= \ e ->
                    pure (let_thunk e, Nothing)

                ANFIR.Expr'MakeADT _ _ variant_index@(Type.ADTVariantIndex adt_key _) args ->
                    mangle_adt adt_key >>= \ adt_mangled ->
                    Type.variant_name <$> (Type.get_adt_variant <$> get_adt_arena <*> pure variant_index) >>= \ variant_name ->
                    zipWithM (\ i arg -> mangle_binding_as_thunk arg >>= \ arg -> pure ("_" <> show (i :: Int), arg)) [0..] args >>= \ object_fields ->
                    pure (default_let_thunk, Just $ set_evaluator "FunctionEvaluator" ("() => new " <> adt_mangled <> "({ " <> Text.intercalate ", " (map (\ (n, a) -> n <> ": " <> a) (("discriminant", "\"" <> variant_name <> "\""):object_fields)) <> " })")) -- TODO

                ANFIR.Expr'Poison _ _ void -> absurd void

        convert_matcher (ANFIR.Switch'BoolLiteral b) = "bool_literal_matcher(" <> if b then "true" else "false" <> ")"
        convert_matcher ANFIR.Switch'Tuple = "tuple_matcher()"
        convert_matcher ANFIR.Switch'Default = "default_matcher()"

stringify_ts_lambda :: TSLambda -> IRReader Text
stringify_ts_lambda (TSLambda key (ANFIR.BindingGroup unique captures _) arg_ty result_ty body_key) =
    refer_type_raw arg_ty >>= \ arg_type_raw ->
    refer_type arg_ty >>= \ arg_type ->
    refer_type_raw result_ty >>= \ result_type_raw ->
    refer_type result_ty >>= \ result_type ->

    Text.intercalate ", " <$>
        mapM
            (\ c ->
                ANFIR.binding_type <$> get_binding c >>= refer_type >>= \ c_ty ->
                mangle_binding_as_capture c >>= \ c_as_capture ->
                pure ("public " <> c_as_capture <> ": " <> c_ty))
            (toList captures) >>= \ capture_constructor_params ->

    mangle_binding_as_lambda key >>= \ lambda_mangled ->
    mangle_binding_as_thunk body_key >>= \ body_as_thunk ->
    mangle_make_thunk_group unique >>= \ make_thunk_graph_for ->
    mapM (\ c -> mangle_binding_as_capture c >>= \ c -> pure ("this." <> c)) (toList captures) >>= \ capture_args ->

    pure ("class " <> lambda_mangled <> " implements Lambda<" <> arg_type_raw <> ", " <> result_type_raw <> "> {\n"
        <> "    constructor(" <> capture_constructor_params  <> ") {}\n"
        <> "    call(arg: " <> arg_type <> "): " <> result_type <> " {\n"
        <> "        return " <> make_thunk_graph_for <> "(" <> Text.intercalate ", " (capture_args ++ ["arg"]) <> ")." <> body_as_thunk <> ";\n"
        <> "    }\n"
        <> "}\n")

stringify_ts_global_thunk :: TSGlobalThunk -> IRReader Text
stringify_ts_global_thunk (TSGlobalThunk key) =
    binding_type key >>= refer_type >>= \ ty ->
    mangle_binding_as_thunk key >>= \ mangled ->
    pure ("let " <> mangled <> ": " <> ty <> ";\n")

initialize_global_thunks :: [TSGlobalThunk] -> IRReader Text
initialize_global_thunks thunks =
    Text.concat <$>
        mapM (\ (TSGlobalThunk k) -> mangle_binding_as_thunk k >>= \ binding_as_thunk -> pure ("    " <> binding_as_thunk <> " = " <> "globals." <> binding_as_thunk <> ";\n")) thunks >>= \ assigns ->
    pure ("function initialize_global_thunks() {\n"
        <> "    let globals = make_global_thunk_graph();\n"
        <> assigns
        <> "}\n"
        <> "initialize_global_thunks();\n")

-- referring to types {{{2
refer_type_raw :: Type.Type Void -> IRReader Text
refer_type_raw (Type.Type'ADT ak params) =
    mangle_adt ak >>= \ ak_mangled ->
    mapM refer_type_raw params >>= \ params_referred ->
    pure (ak_mangled <> " /* #(" <> Text.intercalate ", " params_referred <> ") */") -- not necessary but

refer_type_raw (Type.Type'Synonym sk) =
    get_type_synonym sk >>= \ (Type.TypeSynonym _ _ expansion) -> refer_type expansion

refer_type_raw Type.Type'Int = pure "Int"
refer_type_raw Type.Type'Float = pure "Float"
refer_type_raw Type.Type'Char = pure "Char"
refer_type_raw Type.Type'String = pure "UHFString"
refer_type_raw Type.Type'Bool = pure "Bool"
refer_type_raw (Type.Type'Function a r) = refer_type_raw a >>= \ a -> refer_type_raw r >>= \ r -> pure ("Lambda<" <> a <> ", " <> r <> ">")
refer_type_raw (Type.Type'Tuple a b) = refer_type_raw a >>= \ a -> refer_type_raw b >>= \ b -> pure ("Tuple<" <> a <> ", " <> b <> ">")
refer_type_raw (Type.Type'Unknown void) = absurd void
refer_type_raw (Type.Type'Variable _) = pure "any" -- best approximation
refer_type_raw (Type.Type'Forall _ t) = refer_type_raw t

refer_type :: Type.Type Void -> IRReader Text
refer_type ty = refer_type_raw ty >>= \ ty -> pure ("Thunk<" <> ty <> ">")
-- lowering {{{1
lower :: ANFIR -> Text
lower (ANFIR.ANFIR decls adts type_synonyms type_vars bindings params mod) =
    runReader
        (
            runWriterT (
                define_decl mod (Arena.get decls mod) >>
                Arena.transform_with_keyM define_lambda_type bindings >> -- TODO: do this by tracing bindings from module
                Arena.transform_with_keyM define_binding_group bindings >> -- TODO: also do this by tracing bindings from module
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

define_decl :: ANFIR.DeclKey -> Decl -> TSWriter ()
define_decl _ (ANFIR.Decl'Module global_group adts _) =
    mapM_ (tell_adt . TSADT) adts >>
    mapM (tell_global . TSGlobalThunk) (ANFIR.binding_group_bindings global_group) >>
    tell_make_thunk_graph (TSMakeThunkGraph global_group Nothing) -- global thunk graph does not have any params
define_decl _ (ANFIR.Decl'Type _) = pure ()

define_lambda_type :: ANFIR.BindingKey -> Binding -> TSWriter ()
define_lambda_type key (ANFIR.Binding _ (ANFIR.Expr'Lambda _ _ param group body)) =
    lift (get_param param) >>= \ (ANFIR.Param _ param_ty) ->
    lift (binding_type body) >>= \ body_type ->
    tell_lambda (TSLambda key group param_ty body_type body)
define_lambda_type _ _ = pure ()

define_binding_group :: ANFIR.BindingKey -> Binding -> TSWriter ()
define_binding_group _ (ANFIR.Binding _ (ANFIR.Expr'Lambda _ _ param group _)) = tell_make_thunk_graph (TSMakeThunkGraph group (Just param))
define_binding_group _ (ANFIR.Binding _ (ANFIR.Expr'Switch _ _ _ matchers)) = mapM_ (\ (_, group, _) -> tell_make_thunk_graph (TSMakeThunkGraph group Nothing)) matchers
define_binding_group _ (ANFIR.Binding _ (ANFIR.Expr'Forall _ _ _ group _)) = tell_make_thunk_graph (TSMakeThunkGraph group Nothing)
define_binding_group _ _ = pure ()

-- mangling {{{2
mangle_adt :: Type.ADTKey -> IRReader Text
mangle_adt key = get_adt key >>= \ (Type.ADT id _ _ _) -> pure (ID.mangle id)

mangle_binding_as_lambda :: ANFIR.BindingKey -> IRReader Text
mangle_binding_as_lambda key = ANFIR.binding_id <$> get_binding key >>= \ id -> pure ("Lambda" <> ANFIR.mangle_id id)

mangle_binding_as_capture :: ANFIR.BindingKey -> IRReader Text
mangle_binding_as_capture key = ANFIR.binding_id <$> get_binding key >>= \ id -> pure ("capture" <> ANFIR.mangle_id id)

mangle_binding_as_thunk :: ANFIR.BindingKey -> IRReader Text
mangle_binding_as_thunk key = ANFIR.binding_id <$> get_binding key >>= \ id -> pure ("thunk" <> ANFIR.mangle_id id)

mangle_make_thunk_group :: Unique.Unique -> IRReader Text
mangle_make_thunk_group u = pure $ "make_thunk_group_for_unique_" <> show (Unique.ununique u)
