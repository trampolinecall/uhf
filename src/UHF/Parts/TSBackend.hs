{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyCase #-}

module UHF.Parts.TSBackend (lower) where

import UHF.Prelude

import qualified Data.FileEmbed as FileEmbed
import qualified Data.Set as Set

import qualified UHF.Data.BackendIR as BackendIR
import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Parts.TSBackend.TS as TS
import qualified UHF.Parts.TSBackend.TS.PP as TS.PP
import qualified UHF.Util.Arena as Arena

type CU = BackendIR.CU
type Type = Type.Type
type ADT = Type.ADT Type
type TypeSynonym = Type.TypeSynonym Type
type Binding = BackendIR.Binding Type Void
type BindingGroup = BackendIR.BindingGroup
type Param = BackendIR.Param Type

type BackendIR = BackendIR.BackendIR Type Void

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
-- TODO: dont use VariableKey Ord for order of captures in parameters of function
data TSLambda = TSLambda BackendIR.BindingKey (Set.Set BackendIR.BindingKey) BindingGroup Type Type BackendIR.BindingKey
data TS = TS [TSDecl] [TSADT] [TSLambda] [TS.Stmt]

instance Semigroup TS where
    (TS d1 a1 l1 g1) <> (TS d2 a2 l2 g2) = TS (d1 <> d2) (a1 <> a2) (l1 <> l2) (g1 <> g2)
instance Monoid TS where
    mempty = TS mempty mempty mempty mempty

type TSWriter = WriterT TS IRReader

tell_adt :: TSADT -> TSWriter ()
tell_adt adt = tell $ TS [] [adt] [] []
tell_lambda :: TSLambda -> TSWriter ()
tell_lambda lt = tell $ TS [] [] [lt] []
tell_global_stmt :: TS.Stmt -> TSWriter ()
tell_global_stmt stmt = tell $ TS [] [] [] [stmt]

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
            let name_field = ("discriminant", Just $ TS.Type'StrLit $ ID.mangle $ Type.ADT.variant_id variant)
            in (case variant of
                    Type.ADT.Variant'Named _ _ fields -> mapM (\ (id, _, f) -> (ID.mangle id,) <$> (Just <$> refer_type f)) fields
                    Type.ADT.Variant'Anon _ _ fields -> mapM (\ (id, f) -> (ID.mangle id,) <$> (Just <$> refer_type f)) fields) >>= \ fields ->
            pure (TS.Type'Object $ name_field : fields)

convert_ts_lambda :: TSLambda -> IRReader TS.Stmt
convert_ts_lambda (TSLambda key captures group@(BackendIR.BindingGroup _) arg_ty result_ty body_key) =
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
    mangle_binding_as_var body_key >>= \ body_as_var ->
    mapM
        (\ capture ->
            mangle_binding_as_var capture >>= \ capture_as_var ->
            mangle_binding_as_capture capture >>= \ capture_as_capture ->
            pure (TS.Stmt'Let capture_as_var Nothing (Just $ TS.Expr'Get (TS.Expr'Identifier "this") capture_as_capture)))
        (toList captures) >>= \ make_captures_local ->
    lower_binding_group group >>= \ group_lowered ->

    pure
        (TS.Stmt'Class
            lambda_mangled
            [TS.TypeReference "Lambda" [arg_type_raw, result_type_raw]]
            [ TS.ClassMember'Constructor capture_constructor_params (Just [])
            , TS.ClassMember'MethodDecl "call" [TS.Parameter Nothing "param" (Just arg_type)] (Just result_type)
                (Just $
                    make_captures_local ++
                    group_lowered ++
                    [TS.Stmt'Return $ TS.Expr'Identifier body_as_var])
            ]
        )

-- referring to types {{{2
refer_type_raw :: Type.Type -> IRReader TS.Type
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
refer_type_raw (Type.Type'QuantVar _) = pure $ TS.Type'Reference $ TS.TypeReference "any" [] -- best approximation
refer_type_raw (Type.Type'Forall _ t) = refer_type_raw t
refer_type_raw Type.Type'Kind'Type = pure $ TS.Type'Reference $ TS.TypeReference "any" [] -- not sure what should happen for these 3 here; throw internal error?
refer_type_raw (Type.Type'Kind'Arrow _ _) = pure $ TS.Type'Reference $ TS.TypeReference "any" []
refer_type_raw Type.Type'Kind'Kind = pure $ TS.Type'Reference $ TS.TypeReference "any" []

refer_type :: Type.Type -> IRReader TS.Type
refer_type = refer_type_raw -- may not always be the case
-- lowering {{{1
lower :: BackendIR -> Text
lower (BackendIR.BackendIR adts type_synonyms type_vars bindings params cu) =
    runReader
        (
            runWriterT (
                define_cu cu >>
                Arena.transform_with_keyM define_lambda_type bindings >> -- TODO: do this by tracing bindings from module
                pure ()
            ) >>= \ ((), TS ts_decls ts_adts ts_lambdas ts_global_thunks) ->

            mapM convert_ts_decl ts_decls >>= \ ts_decls ->
            mapM convert_ts_adt ts_adts >>= \ ts_adts ->
            mapM convert_ts_lambda ts_lambdas >>= \ ts_lambdas ->

            pure (runtime_code
                <> TS.PP.stmts
                    (ts_decls
                    <> [TS.Stmt'Spacer]
                    <> ts_adts
                    <> [TS.Stmt'Spacer]
                    <> ts_lambdas
                    <> [TS.Stmt'Spacer]
                    <> ts_global_thunks))
        )
        (adts, type_synonyms, bindings, params)

define_cu :: CU -> TSWriter ()
define_cu (BackendIR.CU global_group adts _) =
    mapM_ (tell_adt . TSADT) adts >>
    lift (lower_binding_group global_group) >>= \ global_init_stmts ->
    mapM_ tell_global_stmt global_init_stmts >>
    pure ()

define_lambda_type :: BackendIR.BindingKey -> Binding -> TSWriter ()
define_lambda_type key (BackendIR.Binding (BackendIR.Expr'Lambda _ _ param captures group body)) =
    lift (get_param param) >>= \ (BackendIR.Param _ param_ty) ->
    lift (binding_type body) >>= \ body_type ->
    tell_lambda (TSLambda key captures group param_ty body_type body)
define_lambda_type _ _ = pure ()

lower_binding_group :: BindingGroup -> IRReader [TS.Stmt]
lower_binding_group (BackendIR.BindingGroup chunks) = concat <$> mapM chunk chunks
    where
        chunk (BackendIR.SingleBinding bk) =
            lower_binding_key bk >>= \ (early, late) ->
            pure (early ++ late)
        chunk (BackendIR.MutuallyRecursiveBindings bks) =
            unzip <$> mapM lower_binding_key bks >>= \ (early, late) ->
            pure (concat early ++ concat late)

        lower_binding_key bk = get_binding bk >>= lower_binding

lower_binding :: Binding -> IRReader ([TS.Stmt], [TS.Stmt])
lower_binding (BackendIR.Binding init) = l init
    where
        l (BackendIR.Expr'Refer id _ other) = mangle_binding_as_var other >>= \ other -> let_current id (TS.Expr'Identifier other) >>= \ let_stmt -> pure ([let_stmt], [])
        l (BackendIR.Expr'Int id _ i) = let_current id (TS.Expr'New (TS.Expr'Identifier "Int") [TS.Expr'Int i]) >>= \ let_stmt -> pure ([let_stmt], [])
        l (BackendIR.Expr'Float id _ (n :% d)) = let_current id (TS.Expr'New (TS.Expr'Identifier "Float") [TS.Expr'Div (TS.Expr'Int n) (TS.Expr'Int d)]) >>= \ let_stmt -> pure ([let_stmt], [])
        l (BackendIR.Expr'Bool id _ b) = let_current id (TS.Expr'New (TS.Expr'Identifier "Bool") [TS.Expr'Bool b]) >>= \ let_stmt -> pure ([let_stmt], [])
        l (BackendIR.Expr'Char id _ c) = let_current id (TS.Expr'New (TS.Expr'Identifier "Char") [TS.Expr'Char c]) >>= \ let_stmt -> pure ([let_stmt], [])
        l (BackendIR.Expr'String id _ s) = let_current id (TS.Expr'New (TS.Expr'Identifier "UHFString") [TS.Expr'String s]) >>= \ let_stmt -> pure ([let_stmt], [])

        l (BackendIR.Expr'Tuple id _ a b) = mangle_binding_as_var a >>= \ a -> mangle_binding_as_var b >>= \ b -> let_current id (TS.Expr'New (TS.Expr'Identifier "Tuple") [TS.Expr'Identifier a, TS.Expr'Identifier b]) >>= \ let_stmt -> pure ([let_stmt], [])
        l (BackendIR.Expr'MakeADT id _ variant_index@(Type.ADT.VariantIndex _ adt_key _) _ args) =
            mangle_adt adt_key >>= \ adt_mangled ->
            Type.ADT.get_variant <$> get_adt_arena <*> pure variant_index >>= \ variant ->
            let variant_id = Type.ADT.variant_id variant
            in zipWithM (\ field_id arg -> mangle_binding_as_var arg >>= \ arg -> pure (ID.mangle field_id, Just $ TS.Expr'Identifier arg)) (Type.ADT.variant_field_ids variant) args >>= \ object_fields ->
            let_current id (TS.Expr'New (TS.Expr'Identifier adt_mangled) [TS.Expr'Object $ ("discriminant", Just $ TS.Expr'StrLit $ ID.mangle variant_id) : object_fields]) >>= \ let_stmt ->
            pure ([let_stmt], [])

        l (BackendIR.Expr'Lambda id _ _ captures _ _) =
            mangle_binding_id_as_var id >>= \ current_var ->
            mangle_binding_id_as_lambda id >>= \ lambda ->
            mapM
                (\ capt ->
                    mangle_binding_as_var capt >>= \ capt_var ->
                    mangle_binding_as_capture capt >>= \ capt_capt ->
                    pure (TS.Stmt'Expr $ TS.Expr'Assign (TS.Expr'Get (TS.Expr'Identifier current_var) capt_capt) (TS.Expr'Identifier capt_var)))
                (toList captures) >>= \ set_captures ->
            let_current id (TS.Expr'New (TS.Expr'Identifier lambda) (map (const (TS.Expr'Identifier "undefined")) (toList captures))) >>= \ let_stmt ->
            pure ([let_stmt], set_captures)
        l (BackendIR.Expr'Param id _ _) = let_current id (TS.Expr'Identifier "param") >>= \ let_stmt -> pure ([let_stmt], [])
        l (BackendIR.Expr'Call id _ callee arg) = mangle_binding_as_var callee >>= \ callee -> mangle_binding_as_var arg >>= \ arg -> let_current id (TS.Expr'Call (TS.Expr'Get (TS.Expr'Identifier callee) "call") [TS.Expr'Identifier arg]) >>= \ let_stmt -> pure ([let_stmt], [])

        l (BackendIR.Expr'Let id _ group result) = do
            store_var <- mangle_binding_id_as_var id
            result <- mangle_binding_as_var result

            let let_store_var = TS.Stmt'Let store_var Nothing Nothing
            binding_group <- lower_binding_group group
            let assign_store_var = TS.Stmt'Expr $ TS.Expr'Assign (TS.Expr'Identifier store_var) (TS.Expr'Identifier result)

            pure (let_store_var : binding_group ++ [assign_store_var], [])

        l (BackendIR.Expr'Match id _ tree) =
            mangle_binding_id_as_var id >>= \ result_var ->
            let label_name = "label_for_match" <> result_var
            in

            let set_result e = [TS.Stmt'Expr $ TS.Expr'Assign (TS.Expr'Identifier result_var) e, TS.Stmt'Break (Just label_name)]
            in

            lower_tree set_result tree >>= \ lowered_tree ->

            pure ([TS.Stmt'Let result_var Nothing Nothing, TS.Stmt'Label label_name lowered_tree], []) -- TODO: check if result is undefined for sanity check even though that should not be possible once exhaustiveness checking is implemented

            where
                lower_tree set_result (BackendIR.MatchTree arms) =
                    TS.Stmt'Block
                        <$> mapM
                            (\ (clauses, result) ->
                                (case result of
                                    Right (group, result) ->
                                        lower_binding_group group >>= \ group_lowered ->
                                        mangle_binding_as_var result >>= \ result ->
                                        pure (TS.Stmt'Block $ group_lowered ++ set_result (TS.Expr'Identifier result))
                                    Left subtree -> lower_tree set_result subtree) >>= \ result ->
                                foldrM lower_clause result clauses
                            )
                            arms

                lower_clause (BackendIR.MatchClause'Match b matcher) result =
                    mangle_binding_as_var b >>= \ b ->
                    convert_matcher (TS.Expr'Identifier b) matcher >>= \ check ->
                    pure (TS.Stmt'If check result Nothing)
                lower_clause (BackendIR.MatchClause'Binding b) result =
                    get_binding b >>= lower_binding >>= \ (early, late) ->
                    pure (TS.Stmt'Block $ early <> late <> [result])

                convert_matcher checking (BackendIR.Match'BoolLiteral b) = pure $ TS.Expr'Eq checking (TS.Expr'Bool b)
                convert_matcher _ BackendIR.Match'Tuple = pure $ TS.Expr'Bool True -- tuple always matches because there is only 1 constructor
                -- TODO: clean this up
                convert_matcher checking (BackendIR.Match'AnonADTVariant (Right variant_index)) =
                    Type.ADT.variant_id <$> (Type.ADT.get_variant <$> get_adt_arena <*> pure variant_index) >>= \ variant_id ->
                    pure (TS.Expr'Eq (TS.Expr'Get (TS.Expr'Get checking "data") "discriminant") (TS.Expr'String $ ID.mangle variant_id))
                convert_matcher _ (BackendIR.Match'AnonADTVariant (Left void)) = absurd void

        l (BackendIR.Expr'TupleDestructure1 id _ tup) = mangle_binding_as_var tup >>= \ tup -> let_current id (TS.Expr'Get (TS.Expr'Identifier tup) "first") >>= \ let_stmt -> pure ([let_stmt], [])
        l (BackendIR.Expr'TupleDestructure2 id _ tup) = mangle_binding_as_var tup >>= \ tup -> let_current id (TS.Expr'Get (TS.Expr'Identifier tup) "second") >>= \ let_stmt -> pure ([let_stmt], [])
        l (BackendIR.Expr'ADTDestructure id _ b (Right field_idx)) =
            mangle_binding_as_var b >>= \ b ->
            Type.ADT.get_field_id <$> get_adt_arena <*> pure field_idx >>= \ field_id ->
            let_current id (TS.Expr'Get (TS.Expr'Get (TS.Expr'Identifier b) "data") (ID.mangle field_id)) >>= \ let_stmt ->
            pure ([let_stmt], [])
        l (BackendIR.Expr'ADTDestructure _ _ _ (Left void)) = absurd void

        -- TODO: lower these 2 properly
        l (BackendIR.Expr'Forall id _ _ group result) =
            mangle_binding_id_as_var id >>= \ current_var ->
            mangle_binding_as_var result >>= \ result ->
            lower_binding_group group >>= \ group_lowered ->
            let final_assign = TS.Stmt'Expr $ TS.Expr'Assign (TS.Expr'Identifier current_var) (TS.Expr'Identifier result)
            in pure ([TS.Stmt'Let current_var Nothing Nothing, TS.Stmt'Block (group_lowered ++ [final_assign])], [])
        l (BackendIR.Expr'TypeApply id _ expr _) = mangle_binding_as_var expr >>= \ expr -> let_current id (TS.Expr'Identifier expr) >>= \ let_stmt -> pure ([let_stmt], [])

        l (BackendIR.Expr'Poison _ _ void) = absurd void

        let_current current_id expr = mangle_binding_id_as_var current_id >>= \ current_id -> pure (TS.Stmt'Let current_id Nothing (Just expr))

-- mangling {{{2
mangle_adt :: Type.ADTKey -> IRReader Text
mangle_adt key = get_adt key >>= \ (Type.ADT id _ _ _) -> pure (ID.mangle id)

mangle_binding_as_lambda :: BackendIR.BindingKey -> IRReader Text
mangle_binding_as_lambda key = BackendIR.binding_id <$> get_binding key >>= mangle_binding_id_as_lambda

mangle_binding_id_as_lambda :: BackendIR.ID -> IRReader Text
mangle_binding_id_as_lambda id = pure ("Lambda" <> BackendIR.mangle_id id)

mangle_binding_as_capture :: BackendIR.BindingKey -> IRReader Text
mangle_binding_as_capture key = BackendIR.binding_id <$> get_binding key >>= \ id -> pure ("capture" <> BackendIR.mangle_id id)

mangle_binding_as_var :: BackendIR.BindingKey -> IRReader Text
mangle_binding_as_var key = BackendIR.binding_id <$> get_binding key >>= mangle_binding_id_as_var

mangle_binding_id_as_var :: BackendIR.ID -> IRReader Text
mangle_binding_id_as_var id = pure ("var" <> BackendIR.mangle_id id)
