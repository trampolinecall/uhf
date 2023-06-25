{-# LANGUAGE EmptyCase #-}

module UHF.Phases.Back.TSBackend (lower) where

import UHF.Util.Prelude

import qualified Arena
import qualified Unique

import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.FileEmbed as FileEmbed

import qualified UHF.Phases.Back.TSBackend.TS as TS
import qualified UHF.Phases.Back.TSBackend.TS.PP as TS.PP

import qualified UHF.Data.IR.BackendIR as BackendIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.ID as ID

type CaptureList = Set.Set BackendIR.BindingKey
type DependencyList = Set.Set BackendIR.BindingKey

type CU = BackendIR.CU CaptureList
type Type = Type.Type Void
type ADT = Type.ADT Type
type TypeSynonym = Type.TypeSynonym Type
type Binding = BackendIR.Binding CaptureList DependencyList Type Void
type BindingGroup = BackendIR.BindingGroup CaptureList
type Param = BackendIR.Param Type

type BackendIR = BackendIR.BackendIR CaptureList DependencyList Type Void

type ADTArena = Arena.Arena ADT Type.ADTKey
type TypeSynonymArena = Arena.Arena TypeSynonym Type.TypeSynonymKey
type BindingArena = Arena.Arena Binding BackendIR.BindingKey
type ParamArena = Arena.Arena Param BackendIR.ParamKey

-- TODO: refactor, rename, reorder, reorganize everything

type IRReader = Reader (ADTArena, TypeSynonymArena, BindingArena, ParamArena)

get_binding :: BackendIR.BindingKey -> IRReader Binding
get_binding k = reader $ \ (_, _, a, _) -> Arena.get a k
get_param :: BackendIR.ParamKey -> IRReader Param
get_param k = reader $ \ (_, _, _, a) -> Arena.get a k
get_adt :: Type.ADTKey -> IRReader ADT
get_adt k = reader $ \ (a, _, _, _) -> Arena.get a k
get_adt_arena :: IRReader ADTArena
get_adt_arena = reader $ \ (a, _, _, _) -> a
get_type_synonym :: Type.TypeSynonymKey -> IRReader TypeSynonym
get_type_synonym k = reader $ \ (_, a, _, _) -> Arena.get a k

binding_type :: BackendIR.BindingKey -> IRReader Type
binding_type k = BackendIR.binding_type <$> get_binding k

-- TS things {{{1
runtime_code :: Text
runtime_code = $(FileEmbed.embedStringFile "data/ts_runtime.ts")

data TSDecl
newtype TSADT = TSADT Type.ADTKey
data TSLambda = TSLambda BackendIR.BindingKey BindingGroup Type Type BackendIR.BindingKey
newtype TSGlobalThunk = TSGlobalThunk BackendIR.BindingKey
-- TODO: dont use BoundValueKey Ord for order of captures in parameters of function
data TSMakeThunkGraph = TSMakeThunkGraph BindingGroup (Maybe BackendIR.ParamKey) -- list of bindings is body, set of bindings is captures
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

convert_ts_decl :: TSDecl -> IRReader TS.Stmt
convert_ts_decl d = case d of {}

convert_ts_adt :: TSADT -> IRReader TS.Stmt
convert_ts_adt (TSADT key) =
    mangle_adt key >>= \ mangled ->
    data_type >>= \ data_type ->
    pure $
        TS.Stmt'Class mangled [] [TS.ClassMember'PropDecl "type" (Just $ TS.Type'StrLit mangled) Nothing, TS.ClassMember'Constructor [TS.Parameter (Just TS.Public) "data" (Just data_type)] (Just [])]

    where
        data_type =
            get_adt key >>= \ (Type.ADT _ _ _ variants) ->
            case variants of
                [] -> pure TS.Type'Never
                first:more ->
                    convert_variant first >>= \ first ->
                    mapM convert_variant more >>= \ more ->
                    pure (foldl' TS.Type'Union first more)

        convert_variant variant =
            let name_field = ("discriminant", Just $ TS.Type'StrLit $ Type.variant_name variant)
            in (case variant of
                    Type.ADTVariant'Named _ fields -> zipWithM (\ i (_, f) -> ("_" <> show (i :: Int),) . Just <$> refer_type f) [0..] fields
                    Type.ADTVariant'Anon _ fields -> zipWithM (\ i f -> ("_" <> show (i :: Int),) . Just <$> refer_type f) [0..] fields) >>= \ fields ->
            pure (TS.Type'Object $ name_field : fields)

convert_ts_make_thunk_graph :: TSMakeThunkGraph -> IRReader TS.Stmt
convert_ts_make_thunk_graph (TSMakeThunkGraph (BackendIR.BindingGroup unique captures included_bindings) param) =
    mapM convert_param param >>= \ converted_param ->
    mapM convert_capture (Set.toList captures) >>= \ converted_captures ->

    unzip <$> mapM convert_binding_decl included_bindings >>= \ (binding_decls, binding_set_evaluators) ->
    mangle_make_thunk_group unique >>= \ fn_name ->
    ts_return_type >>= \ ts_return_type ->
    object_of_bindings >>= \ object_of_bindings ->

    pure
        (TS.Stmt'Function
            fn_name
            (converted_captures ++ Maybe.maybeToList converted_param)
            (Just ts_return_type)
            (Just $
                binding_decls
                    <> [TS.Stmt'Spacer]
                    <> Maybe.catMaybes binding_set_evaluators
                    <> [TS.Stmt'Spacer]
                    <> [TS.Stmt'Return object_of_bindings]))
    where
        ts_return_type =
            mapM r included_bindings >>= \ fields ->
            pure (TS.Type'Object fields)
            where
                r binding =
                    binding_type binding >>= refer_type >>= \ ty ->
                    mangle_binding_as_thunk binding >>= \ mangled ->
                    pure (mangled, Just ty)

        object_of_bindings =
            mapM (fmap (, Nothing) . mangle_binding_as_thunk) included_bindings >>= \ contents ->
            pure (TS.Expr'Object contents)

        convert_param param_key =
            get_param param_key >>= \ (BackendIR.Param _ param_ty) ->
            refer_type param_ty >>= \ ty_refer ->
            pure (TS.Parameter Nothing "param" (Just ty_refer))

        convert_capture bk =
            BackendIR.binding_type <$> get_binding bk >>= refer_type >>= \ ty_refer ->
            mangle_binding_as_thunk bk >>= \ mangled ->
            pure (TS.Parameter Nothing mangled (Just ty_refer))

        convert_binding_decl binding_key =
            binding_type binding_key >>= refer_type >>= \ cur_binding_type ->
            mangle_binding_as_thunk binding_key >>= \ binding_as_thunk ->
            let set_evaluator evaluator_name evaluator_args = TS.Stmt'Expr $ TS.Expr'Assign (TS.Expr'Get (TS.Expr'Identifier binding_as_thunk) "evaluator") (TS.Expr'New (TS.Expr'Identifier evaluator_name) evaluator_args)
                let_thunk initializer = TS.Stmt'Let binding_as_thunk (Just cur_binding_type) (Just initializer)
                default_let_thunk = let_thunk (TS.Expr'New (TS.Expr'Identifier "Thunk") [TS.Expr'Undefined])

            in BackendIR.binding_initializer <$> get_binding binding_key >>= \case
                BackendIR.Expr'Refer _ _ i ->
                    mangle_binding_as_thunk i >>= \ i_mangled ->
                    pure (default_let_thunk, Just (set_evaluator "PassthroughEvaluator" [TS.Expr'Identifier i_mangled]))
                BackendIR.Expr'Int _ _ i -> pure (default_let_thunk, Just (set_evaluator "ConstEvaluator" [TS.Expr'New (TS.Expr'Identifier "Int") [TS.Expr'Int i]]))
                BackendIR.Expr'Float _ _ (num :% denom) -> pure (default_let_thunk, Just (set_evaluator "ConstEvaluator" [TS.Expr'New (TS.Expr'Identifier "Float") [TS.Expr'Div (TS.Expr'Int num) (TS.Expr'Int denom)]]))
                BackendIR.Expr'Bool _ _ b -> pure (default_let_thunk, Just (set_evaluator "ConstEvaluator" [TS.Expr'New (TS.Expr'Identifier "Bool") [TS.Expr'Bool b]]))
                BackendIR.Expr'Char _ _ c -> pure (default_let_thunk, Just (set_evaluator "ConstEvaluator" [TS.Expr'New (TS.Expr'Identifier "Char") [TS.Expr'Char c]]))
                BackendIR.Expr'String _ _ s -> pure (default_let_thunk, Just (set_evaluator "ConstEvaluator" [TS.Expr'New (TS.Expr'Identifier "UHFString") [TS.Expr'String s]]))
                BackendIR.Expr'Tuple _ _ a b ->
                    mangle_binding_as_thunk a >>= \ a_mangled ->
                    mangle_binding_as_thunk b >>= \ b_mangled ->
                    pure (default_let_thunk, Just (set_evaluator "TupleEvaluator" [TS.Expr'Identifier a_mangled, TS.Expr'Identifier b_mangled]))

                BackendIR.Expr'Lambda _ _ _ group _ ->
                    let lambda_captures = BackendIR.binding_group_captures group
                    in mangle_binding_as_lambda binding_key >>= \ lambda ->
                    mapM (fmap TS.Expr'Identifier . mangle_binding_as_thunk) (toList lambda_captures) >>= \ lambda_captures_mangled ->
                    pure (default_let_thunk, Just (set_evaluator "ConstEvaluator" [TS.Expr'New (TS.Expr'Identifier lambda) lambda_captures_mangled]))
                BackendIR.Expr'Param _ _ _ -> pure (let_thunk (TS.Expr'Identifier "param"), Nothing)

                BackendIR.Expr'Call _ _ callee arg ->
                    mangle_binding_as_thunk callee >>= \ callee_mangled ->
                    mangle_binding_as_thunk arg >>= \ arg_mangled ->
                    pure (default_let_thunk, Just (set_evaluator "CallEvaluator" [TS.Expr'Identifier callee_mangled, TS.Expr'Identifier arg_mangled]))

                BackendIR.Expr'Switch _ _ test arms ->
                    mapM (\ (matcher, group, res) -> -- TODO: lower binding group
                        mangle_make_thunk_group (BackendIR.binding_group_unique group) >>= \ make_group ->
                        mangle_binding_as_thunk res >>= \ res' ->
                        pure (TS.Expr'List [convert_matcher matcher, TS.Expr'Get (TS.Expr'Call (TS.Expr'Identifier make_group) []) res'])) arms >>= \ arms' -> -- TODO: pass captures
                    mangle_binding_as_thunk test >>= \ test_mangled ->
                    pure (default_let_thunk, Just (set_evaluator "SwitchEvaluator" [TS.Expr'Identifier test_mangled, TS.Expr'List arms']))

                BackendIR.Expr'Seq _ _ a b ->
                    mangle_binding_as_thunk a >>= \ a_mangled ->
                    mangle_binding_as_thunk b >>= \ b_mangled ->
                    pure (default_let_thunk, Just (set_evaluator "SeqEvaluator" [TS.Expr'Identifier a_mangled, TS.Expr'Identifier b_mangled]))

                BackendIR.Expr'TupleDestructure1 _ _ tup ->
                    mangle_binding_as_thunk tup >>= \ tup_mangled ->
                    pure (default_let_thunk, Just (set_evaluator "TupleDestructure1Evaluator" [TS.Expr'Identifier tup_mangled]))
                BackendIR.Expr'TupleDestructure2 _ _ tup ->
                    mangle_binding_as_thunk tup >>= \ tup_mangled ->
                    pure (default_let_thunk, Just (set_evaluator "TupleDestructure2Evaluator" [TS.Expr'Identifier tup_mangled]))

                -- foralls and type applications get erased, TODO: explain this better and also reconsider if this is actually correct
                BackendIR.Expr'Forall _ _ _ bg e ->
                    mangle_make_thunk_group (BackendIR.binding_group_unique bg) >>= \ make_bg ->
                    mangle_binding_as_thunk e >>= \ e ->
                    pure (let_thunk $ TS.Expr'Get (TS.Expr'Call (TS.Expr'Identifier make_bg) []) e, Nothing) -- TODO: pass captures
                BackendIR.Expr'TypeApply _ _ e _ ->
                    mangle_binding_as_thunk e >>= \ e ->
                    pure (let_thunk (TS.Expr'Identifier e), Nothing)

                BackendIR.Expr'MakeADT _ _ variant_index@(Type.ADTVariantIndex adt_key _) args ->
                    mangle_adt adt_key >>= \ adt_mangled ->
                    Type.variant_name <$> (Type.get_adt_variant <$> get_adt_arena <*> pure variant_index) >>= \ variant_name ->
                    zipWithM (\ i arg -> mangle_binding_as_thunk arg >>= \ arg -> pure ("_" <> show (i :: Int), Just $ TS.Expr'Identifier arg)) [0..] args >>= \ object_fields ->
                    pure (default_let_thunk, Just $ set_evaluator "FunctionEvaluator" [TS.Expr'ArrowFunction [] Nothing (Left $ TS.Expr'New (TS.Expr'Identifier adt_mangled) [TS.Expr'Object $ ("discriminant", Just $ TS.Expr'StrLit variant_name) : object_fields])]) -- TODO

                BackendIR.Expr'Poison _ _ void -> absurd void

        convert_matcher (BackendIR.Switch'BoolLiteral b) = TS.Expr'Call (TS.Expr'Identifier "bool_literal_matcher") [TS.Expr'Bool b]
        convert_matcher BackendIR.Switch'Tuple = TS.Expr'Call (TS.Expr'Identifier "tuple_matcher") []
        convert_matcher BackendIR.Switch'Default = TS.Expr'Call (TS.Expr'Identifier "default_matcher") []

convert_ts_lambda :: TSLambda -> IRReader TS.Stmt
convert_ts_lambda (TSLambda key (BackendIR.BindingGroup unique captures _) arg_ty result_ty body_key) =
    refer_type_raw arg_ty >>= \ arg_type_raw ->
    refer_type arg_ty >>= \ arg_type ->
    refer_type_raw result_ty >>= \ result_type_raw ->
    refer_type result_ty >>= \ result_type ->

    mapM
        (\ c ->
            BackendIR.binding_type <$> get_binding c >>= refer_type >>= \ c_ty ->
            mangle_binding_as_capture c >>= \ c_as_capture ->
            pure (TS.Parameter (Just TS.Public) c_as_capture (Just c_ty)))
        (toList captures) >>= \ capture_constructor_params ->

    mangle_binding_as_lambda key >>= \ lambda_mangled ->
    mangle_binding_as_thunk body_key >>= \ body_as_thunk ->
    mangle_make_thunk_group unique >>= \ make_thunk_graph_for ->
    mapM (\ c -> mangle_binding_as_capture c >>= \ c -> pure (TS.Expr'Get (TS.Expr'Identifier "this") c)) (toList captures) >>= \ capture_args ->

    pure
        (TS.Stmt'Class
            lambda_mangled
            [TS.TypeReference "Lambda" [arg_type_raw, result_type_raw]]
            [ TS.ClassMember'Constructor capture_constructor_params (Just [])
            , TS.ClassMember'MethodDecl "call" [TS.Parameter Nothing "arg" (Just arg_type)] (Just result_type)
                (Just [TS.Stmt'Return $ TS.Expr'Get (TS.Expr'Call (TS.Expr'Identifier make_thunk_graph_for) (capture_args ++ [TS.Expr'Identifier "arg"])) body_as_thunk])
            ]
        )

convert_ts_global_thunk :: TSGlobalThunk -> IRReader TS.Stmt
convert_ts_global_thunk (TSGlobalThunk key) =
    binding_type key >>= refer_type >>= \ ty ->
    mangle_binding_as_thunk key >>= \ mangled ->
    pure (TS.Stmt'Let mangled (Just ty) Nothing)

initialize_global_thunks :: [TSGlobalThunk] -> IRReader [TS.Stmt]
initialize_global_thunks thunks =
    mapM
        (\ (TSGlobalThunk k) ->
            mangle_binding_as_thunk k >>= \ binding_as_thunk ->
            pure (TS.Stmt'Expr $ TS.Expr'Assign (TS.Expr'Identifier binding_as_thunk) (TS.Expr'Get (TS.Expr'Identifier "globals") binding_as_thunk)))
        thunks >>= \ assigns ->
    pure
        [ TS.Stmt'Function "initialize_global_thunks" [] Nothing (Just $ TS.Stmt'Let "globals" Nothing (Just $ TS.Expr'Call (TS.Expr'Identifier "make_global_thunk_graph") []) : assigns)
        , TS.Stmt'Expr $ TS.Expr'Call (TS.Expr'Identifier "initialize_global_thunks") []
        ]

-- referring to types {{{2
refer_type_raw :: Type.Type Void -> IRReader TS.Type
refer_type_raw (Type.Type'ADT ak _) = -- type parameters erased
    mangle_adt ak >>= \ ak_mangled ->
    pure (TS.Type'Reference (TS.TypeReference ak_mangled []))

refer_type_raw (Type.Type'Synonym sk) =
    get_type_synonym sk >>= \ (Type.TypeSynonym _ _ expansion) -> refer_type expansion

refer_type_raw Type.Type'Int = pure $ TS.Type'Reference $ TS.TypeReference "Int" []
refer_type_raw Type.Type'Float = pure $ TS.Type'Reference $ TS.TypeReference "Float" []
refer_type_raw Type.Type'Char = pure $ TS.Type'Reference $ TS.TypeReference "Char" []
refer_type_raw Type.Type'String = pure $ TS.Type'Reference $ TS.TypeReference "UHFString" []
refer_type_raw Type.Type'Bool = pure $ TS.Type'Reference $ TS.TypeReference "Bool" []
refer_type_raw (Type.Type'Function a r) = refer_type_raw a >>= \ a -> refer_type_raw r >>= \ r -> pure (TS.Type'Reference $ TS.TypeReference "Lambda" [a, r])
refer_type_raw (Type.Type'Tuple a b) = refer_type_raw a >>= \ a -> refer_type_raw b >>= \ b -> pure (TS.Type'Reference $ TS.TypeReference "Tuple" [a, b])
refer_type_raw (Type.Type'Unknown void) = absurd void
refer_type_raw (Type.Type'Variable _) = pure $ TS.Type'Reference $ TS.TypeReference "any" [] -- best approximation
refer_type_raw (Type.Type'Forall _ t) = refer_type_raw t

refer_type :: Type.Type Void -> IRReader TS.Type
refer_type ty = refer_type_raw ty >>= \ ty -> pure (TS.Type'Reference $ TS.TypeReference "Thunk" [ty])
-- lowering {{{1
lower :: BackendIR -> Text
lower (BackendIR.BackendIR adts type_synonyms type_vars bindings params cu) =
    runReader
        (
            runWriterT (
                define_cu cu >>
                Arena.transform_with_keyM define_lambda_type bindings >> -- TODO: do this by tracing bindings from module
                Arena.transform_with_keyM define_binding_group bindings >> -- TODO: also do this by tracing bindings from module
                pure ()
            ) >>= \ ((), TS ts_decls ts_adts ts_make_thunk_graphs ts_lambdas ts_global_thunks) ->

            mapM convert_ts_decl ts_decls >>= \ ts_decls ->
            mapM convert_ts_adt ts_adts >>= \ ts_adts ->
            mapM convert_ts_make_thunk_graph ts_make_thunk_graphs >>= \ ts_make_thunk_graphs ->
            mapM convert_ts_lambda ts_lambdas >>= \ ts_lambdas ->
            initialize_global_thunks ts_global_thunks >>= \ initialize_global_thunks ->
            mapM convert_ts_global_thunk ts_global_thunks >>= \ ts_global_thunks ->

            pure (runtime_code
                <> TS.PP.stmts
                    (ts_decls
                    <> [TS.Stmt'Spacer]
                    <> ts_adts
                    <> [TS.Stmt'Spacer]
                    <> ts_global_thunks
                    <> [TS.Stmt'Spacer]
                    <> ts_make_thunk_graphs
                    <> [TS.Stmt'Spacer]
                    <> ts_lambdas
                    <> [TS.Stmt'Spacer]
                    <> initialize_global_thunks))
        )
        (adts, type_synonyms, bindings, params)

define_cu :: CU -> TSWriter ()
define_cu (BackendIR.CU global_group adts _) =
    mapM_ (tell_adt . TSADT) adts >>
    mapM (tell_global . TSGlobalThunk) (BackendIR.binding_group_bindings global_group) >>
    tell_make_thunk_graph (TSMakeThunkGraph global_group Nothing) -- global thunk graph does not have any params

define_lambda_type :: BackendIR.BindingKey -> Binding -> TSWriter ()
define_lambda_type key (BackendIR.Binding _ _ (BackendIR.Expr'Lambda _ _ param group body)) =
    lift (get_param param) >>= \ (BackendIR.Param _ param_ty) ->
    lift (binding_type body) >>= \ body_type ->
    tell_lambda (TSLambda key group param_ty body_type body)
define_lambda_type _ _ = pure ()

define_binding_group :: BackendIR.BindingKey -> Binding -> TSWriter ()
define_binding_group _ (BackendIR.Binding _ _ (BackendIR.Expr'Lambda _ _ param group _)) = tell_make_thunk_graph (TSMakeThunkGraph group (Just param))
define_binding_group _ (BackendIR.Binding _ _ (BackendIR.Expr'Switch _ _ _ matchers)) = mapM_ (\ (_, group, _) -> tell_make_thunk_graph (TSMakeThunkGraph group Nothing)) matchers
define_binding_group _ (BackendIR.Binding _ _ (BackendIR.Expr'Forall _ _ _ group _)) = tell_make_thunk_graph (TSMakeThunkGraph group Nothing)
define_binding_group _ _ = pure ()

-- mangling {{{2
mangle_adt :: Type.ADTKey -> IRReader Text
mangle_adt key = get_adt key >>= \ (Type.ADT id _ _ _) -> pure (ID.mangle id)

mangle_binding_as_lambda :: BackendIR.BindingKey -> IRReader Text
mangle_binding_as_lambda key = BackendIR.binding_id <$> get_binding key >>= \ id -> pure ("Lambda" <> BackendIR.mangle_id id)

mangle_binding_as_capture :: BackendIR.BindingKey -> IRReader Text
mangle_binding_as_capture key = BackendIR.binding_id <$> get_binding key >>= \ id -> pure ("capture" <> BackendIR.mangle_id id)

mangle_binding_as_thunk :: BackendIR.BindingKey -> IRReader Text
mangle_binding_as_thunk key = BackendIR.binding_id <$> get_binding key >>= \ id -> pure ("thunk" <> BackendIR.mangle_id id)

mangle_make_thunk_group :: Unique.Unique -> IRReader Text
mangle_make_thunk_group u = pure $ "make_thunk_group_for_unique_" <> show (Unique.ununique u)
