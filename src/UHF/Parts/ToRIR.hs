module UHF.Parts.ToRIR (Error, convert) where

import UHF.Prelude

import qualified Data.Map as Map

import UHF.Source.Located (Located (Located, unlocate, just_span))
import UHF.Source.Span (Span)
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.RIR as RIR
import qualified UHF.Data.SIR as SIR
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Parts.ToRIR.PatternCheck as PatternCheck
import qualified UHF.Parts.ToRIR.TopologicalSort as TopologicalSort
import qualified UHF.Util.Arena as Arena
import qualified UHF.Util.IDGen as IDGen

type Type = Maybe Type.Type

type DIden = Maybe (SIR.Decl Type.Type)
type VIden = Maybe SIR.BoundValue
type PIden = Maybe Type.ADT.VariantIndex

type LastSIR = (DIden, DIden, Type, VIden, VIden, PIden, PIden, Type, Void)

type VariableArena = Arena.Arena RIR.Variable RIR.VariableKey

data Error
    = CompletenessError (PatternCheck.CompletenessError LastSIR)
    | HasLoops TopologicalSort.BindingsHaveLoopError

instance Diagnostic.ToError Error where
    to_error (CompletenessError c) = Diagnostic.to_error c
    to_error (HasLoops h) = Diagnostic.to_error h

type ConvertState = ReaderT (Arena.Arena (Type.ADT Type) Type.ADTKey, Arena.Arena (Type.TypeSynonym Type) Type.TypeSynonymKey) (StateT VariableArena (IDGen.IDGenT ID.VariableID (IDGen.IDGenT ID.ExprID (Compiler.WithDiagnostics Error (PatternCheck.NotUseful LastSIR)))))

new_made_up_expr_id :: (ID.ExprID -> a) -> ConvertState a
new_made_up_expr_id make =
    lift (lift $ lift IDGen.gen_id) >>= \ id ->
    pure (make id)

convert :: SIR.SIR LastSIR -> Compiler.WithDiagnostics Error (PatternCheck.NotUseful LastSIR) RIR.RIR
convert (SIR.SIR modules adts type_synonyms quant_vars vars (SIR.CU root_module main_function)) = do
    let adts_converted = Arena.transform convert_adt adts
    let type_synonyms_converted = Arena.transform convert_type_synonym type_synonyms
    let vars_converted = Arena.transform (\ (SIR.Variable id ty (Located sp _)) -> RIR.Variable id ty sp) vars
    (cu, vars_with_new) <- IDGen.run_id_gen_t ID.ExprID'RIRGen $ IDGen.run_id_gen_t ID.VariableID'RIRMadeUp $ runStateT (runReaderT (convert_root_module main_function (Arena.get modules root_module)) (adts_converted, type_synonyms_converted)) vars_converted
    pure (RIR.RIR adts_converted type_synonyms_converted quant_vars vars_with_new cu)

convert_root_module :: Maybe SIR.VariableKey -> SIR.Module LastSIR -> ConvertState RIR.CU
convert_root_module main_function (SIR.Module _ bindings adts type_synonyms) = do
    adt_constructors <- adts
        & mapM ( \ adt_key -> do
            (adts, _) <- ask
            let variants = Type.ADT.variant_idxs adts adt_key
            mapM (\ variant_idx -> (variant_idx,) <$> make_adt_constructor variant_idx) variants
        )
        & fmap concat
    let adt_constructor_map = Map.fromList $ map (\ (variant_index, (var_key, _)) -> (variant_index, var_key)) adt_constructors

    bindings <- concat <$> runReaderT (mapM convert_binding bindings) adt_constructor_map
    bindings_sorted <- sort_bindings (bindings <> map (\ (_, (_, binding)) -> binding) adt_constructors)
    pure (RIR.CU bindings_sorted adts type_synonyms main_function)

make_adt_constructor :: Type.ADT.VariantIndex -> ConvertState (RIR.VariableKey, RIR.Binding)
make_adt_constructor variant_index@(Type.ADT.VariantIndex _ adt_key _) = do
    (adts, _) <- ask
    let (Type.ADT _ _ adt_quant_vars _) = Arena.get adts adt_key
    let variant = Type.ADT.get_variant adts variant_index
    let variant_name_sp = just_span $ Type.ADT.variant_name variant

    -- TODO: do not use the adt quant variables? duplicate them instead?
    let wrap_in_forall = case adt_quant_vars of
            [] -> pure
            param:more -> \ lambda -> new_made_up_expr_id (\ id -> RIR.Expr'Forall id variant_name_sp (param :| more) lambda)

    let make_lambdas type_params variant_index refer_to_params [] =
            let ty_params_as_tys = map Type.Type'QuantVar type_params
            in new_made_up_expr_id $ \ id -> RIR.Expr'MakeADT id variant_name_sp variant_index (map Just ty_params_as_tys) refer_to_params

        make_lambdas type_params variant_index refer_to_params (cur_field_ty:more_field_tys) =
            new_variable cur_field_ty variant_name_sp >>= \ param_var_key ->
            new_made_up_expr_id (\ id -> RIR.Expr'Refer id cur_field_ty variant_name_sp (Just param_var_key)) >>= \ refer_expr ->

            make_lambdas type_params variant_index (refer_to_params <> [refer_expr]) more_field_tys >>= \ lambda_result ->
            new_made_up_expr_id (\ id -> RIR.Expr'Lambda id variant_name_sp param_var_key (TopologicalSort.get_captures param_var_key lambda_result) lambda_result)

    lambdas <- make_lambdas adt_quant_vars variant_index [] (Type.ADT.variant_field_types variant) >>= wrap_in_forall

    var_arena <- lift get
    var_key <- new_variable (RIR.expr_type var_arena lambdas) variant_name_sp
    pure (var_key, RIR.Binding var_key lambdas)

convert_adt :: SIR.ADT LastSIR -> Type.ADT Type
convert_adt (Type.ADT id name quant_vars variants) = Type.ADT id name quant_vars (map convert_variant variants)
    where
        convert_variant (Type.ADT.Variant'Named name id fields) = Type.ADT.Variant'Named name id (map (\ (id, name, (_, ty)) -> (id, name, ty)) fields)
        convert_variant (Type.ADT.Variant'Anon name id fields) = Type.ADT.Variant'Anon name id (map (\ (id, (_, ty)) -> (id, ty)) fields)

convert_type_synonym :: SIR.TypeSynonym LastSIR -> Type.TypeSynonym Type
convert_type_synonym (Type.TypeSynonym id name (_, expansion)) = Type.TypeSynonym id name expansion

convert_binding :: SIR.Binding LastSIR -> ReaderT (Map Type.ADT.VariantIndex RIR.VariableKey) ConvertState [RIR.Binding]
convert_binding (SIR.Binding pat eq_sp expr) = convert_expr expr >>= lift . assign_pattern eq_sp pat

new_made_up_var_id :: ConvertState ID.VariableID
new_made_up_var_id = lift $ lift IDGen.gen_id
new_variable :: Type -> Span -> ConvertState SIR.VariableKey
new_variable ty sp =
    new_made_up_var_id >>= \ id ->
    lift (state $ Arena.put (RIR.Variable id ty sp))

convert_expr :: SIR.Expr LastSIR -> ReaderT (Map Type.ADT.VariantIndex RIR.VariableKey) ConvertState RIR.Expr
convert_expr (SIR.Expr'Refer id ty sp _ bv) = do
    adt_constructor_map <- ask
    case bv of
        Just (SIR.BoundValue'Variable var) -> pure $ RIR.Expr'Refer id ty sp (Just var)
        Just (SIR.BoundValue'ADTVariantConstructor variant) -> pure $ RIR.Expr'Refer id ty sp (Just $ adt_constructor_map Map.! variant) -- TODO: lower these on demand to really ensure that this cannot be partial?
        Just (SIR.BoundValue'Intrinsic i) -> pure $ RIR.Expr'Intrinsic id ty sp i
        Nothing -> pure $ RIR.Expr'Refer id ty sp Nothing
convert_expr (SIR.Expr'Char id ty sp c) = pure $ RIR.Expr'Char id sp c
convert_expr (SIR.Expr'String id ty sp s) = pure $ RIR.Expr'String id sp s
convert_expr (SIR.Expr'Int id ty sp i) = pure $ RIR.Expr'Int id sp i
convert_expr (SIR.Expr'Float id ty sp f) = pure $ RIR.Expr'Float id sp f
convert_expr (SIR.Expr'Bool id ty sp b) = pure $ RIR.Expr'Bool id sp b
convert_expr (SIR.Expr'Tuple id ty sp a b) = RIR.Expr'Tuple id sp <$> convert_expr a <*> convert_expr b
convert_expr (SIR.Expr'Lambda id ty sp param_pat body) =
    let param_ty = SIR.pattern_type param_pat
        body_sp = SIR.expr_span body
    in
    -- '\ (...) -> body' becomes '\ (arg) -> let ... = arg; body'
    lift (new_variable param_ty (SIR.pattern_span param_pat)) >>= \ param_bk ->
    lift (assign_pattern (SIR.pattern_span param_pat) param_pat (RIR.Expr'Refer id param_ty (SIR.pattern_span param_pat) (Just param_bk))) >>= \ bindings ->
    RIR.Expr'Let id body_sp <$> lift (sort_bindings bindings) <*> pure [] <*> pure [] <*> convert_expr body >>= \ body ->
    pure (RIR.Expr'Lambda id sp param_bk (TopologicalSort.get_captures param_bk body) body)

convert_expr (SIR.Expr'Let id ty sp bindings adts type_synonyms body) = RIR.Expr'Let id sp <$> (concat <$> mapM convert_binding bindings >>= lift . sort_bindings) <*> pure adts <*> pure type_synonyms <*> convert_expr body -- TODO: define adt constructors for these
convert_expr (SIR.Expr'LetRec id ty sp bindings adts type_synonyms body) = RIR.Expr'Let id sp <$> (concat <$> mapM convert_binding bindings >>= lift . sort_bindings) <*> pure adts <*> pure type_synonyms <*> convert_expr body -- TODO: define adt constructors for these
convert_expr (SIR.Expr'BinaryOps _ void _ _ _ _) = absurd void
convert_expr (SIR.Expr'Call id ty sp callee arg) = RIR.Expr'Call id sp <$> convert_expr callee <*> convert_expr arg
convert_expr (SIR.Expr'If id ty sp _ cond true false) =
    -- if C then T else F
    -- becomes
    -- let cond = C;
    -- case {
    --     [cond -> true] -> T;
    --     [cond -> false] -> F;
    -- }

    convert_expr cond >>= \ cond ->
    convert_expr true >>= \ true ->
    convert_expr false >>= \ false ->

    lift (lift get) >>= \ var_arena ->
    lift (new_variable (RIR.expr_type var_arena cond) (RIR.expr_span cond)) >>= \ cond_var ->

    lift (sort_bindings [RIR.Binding cond_var cond]) >>= \ cond_bindings ->
    lift $ new_made_up_expr_id
        (\ let_id ->
            RIR.Expr'Let let_id sp
                cond_bindings
                []
                []
                (RIR.Expr'Match id ty sp
                    (RIR.MatchTree
                        [ ([RIR.MatchClause'Match cond_var (RIR.Match'BoolLiteral True)], Right true)
                        , ([RIR.MatchClause'Match cond_var (RIR.Match'BoolLiteral False)], Right false)
                        ]
                    )
                )
        )

convert_expr (SIR.Expr'Match id ty sp match_tok_sp scrutinee arms) = do
    -- case S {
    --     P -> ...;
    --     ...
    -- }
    -- becomes
    -- let scrutinee = S
    -- case {
    --     [scrutinee -> P] -> ...;
    --     ...
    -- }

    scrutinee <- convert_expr scrutinee
    var_arena <- lift $ lift get
    scrutinee_var <- lift $ new_variable (RIR.expr_type var_arena scrutinee) (RIR.expr_span scrutinee)

    (adt_arena, type_synonym_arena) <- lift ask
    case PatternCheck.check_complete adt_arena type_synonym_arena match_tok_sp (map fst arms) of
        Right () -> pure ()
        Left err -> lift (lift $ lift $ lift $ lift $ Compiler.tell_error $ CompletenessError err) >> pure ()
    case PatternCheck.check_useful adt_arena type_synonym_arena (map fst arms) of
        Right () -> pure ()
        Left warns -> lift $ lift $ lift $ lift $ lift $ Compiler.tell_warnings warns

    arms <- arms
        & mapM (\ (pat, result) ->
            lift (pattern_to_clauses scrutinee_var pat) >>= \ clauses ->
            convert_expr result >>= \ result ->
            pure (clauses, Right result)
        )

    scrutinee_bindings <- lift $ sort_bindings [RIR.Binding scrutinee_var scrutinee]
    lift $ new_made_up_expr_id
        (\ let_id ->
            RIR.Expr'Let let_id sp
                scrutinee_bindings
                []
                []
                (RIR.Expr'Match id ty sp (RIR.MatchTree arms))
        )
    where
        pattern_to_clauses ::
            SIR.VariableKey
                      -> SIR.Pattern LastSIR
                      -> ConvertState [RIR.MatchClause]
        pattern_to_clauses scrutinee_var (SIR.Pattern'Variable _ _ var_key) = pure [RIR.MatchClause'Assign var_key (RIR.MatchAssignRHS'OtherVar scrutinee_var)]
        pattern_to_clauses _ (SIR.Pattern'Wildcard _ _) = pure []
        pattern_to_clauses scrutinee_var (SIR.Pattern'Tuple _ _ a b) =
            -- scrutinee -> (A, B) becomes [scrutinee -> (,), a = scrutinee.tuple_l, b = scrutinee.tuple_r, a -> A, b -> B]
            new_variable (SIR.pattern_type a) (SIR.pattern_span a) >>= \ a_var ->
            new_variable (SIR.pattern_type b) (SIR.pattern_span b) >>= \ b_var ->

            pattern_to_clauses a_var a >>= \ a_subpat_matchers ->
            pattern_to_clauses b_var b >>= \ b_subpat_matchers ->

            pure (RIR.MatchClause'Match scrutinee_var RIR.Match'Tuple : RIR.MatchClause'Assign a_var (RIR.MatchAssignRHS'TupleDestructure1 (SIR.pattern_type a) scrutinee_var) : RIR.MatchClause'Assign b_var (RIR.MatchAssignRHS'TupleDestructure2 (SIR.pattern_type b) scrutinee_var) : (a_subpat_matchers <> b_subpat_matchers))

        pattern_to_clauses scrutinee_var (SIR.Pattern'Named _ _ _ (Located _ var_key) subpat) =
            -- scrutinee -> name@P becomes [name = scrutinee, scrutinee -> P]
            pattern_to_clauses scrutinee_var subpat >>= \ subpat_matchers ->
            pure (RIR.MatchClause'Assign var_key (RIR.MatchAssignRHS'OtherVar scrutinee_var) : subpat_matchers)

        pattern_to_clauses scrutinee_var (SIR.Pattern'AnonADTVariant ty sp _ variant_index tyargs fields) =
            -- Variant(F0, F1, ...) becomes [scrutinee -> Variant, f0 = (scrutinee as Variant).0, f1 = (scrutinee as Variant).1, f0 -> F0, f1 -> F1, ...]
            fields & mapM (\ pat -> new_variable (SIR.pattern_type pat) (SIR.pattern_span pat)) >>= \ field_vars ->
            zipWithM pattern_to_clauses field_vars fields >>= \ field_subpat_clauses ->
            ask >>= \ (adt_arena, _) ->
            let field_idxs = Type.ADT.variant_field_idxs adt_arena <$> variant_index
            in
            pure (
                RIR.MatchClause'Match scrutinee_var (RIR.Match'AnonADTVariant variant_index)
                    : case field_idxs of
                        Just field_idxs ->
                            if length field_idxs /= length field_vars
                                then error "not the right number fields in a anon adt variant pattern" -- sanity check assertion
                            else zipWith3
                                (\ field_idx field_var field_pat -> RIR.MatchClause'Assign field_var (RIR.MatchAssignRHS'AnonADTVariantField (SIR.pattern_type field_pat) scrutinee_var (Just field_idx)))
                                field_idxs
                                field_vars
                                fields
                        Nothing ->
                            zipWith
                                (\ field_var field_pat -> RIR.MatchClause'Assign field_var (RIR.MatchAssignRHS'AnonADTVariantField (SIR.pattern_type field_pat) scrutinee_var Nothing))
                                field_vars
                                fields
                    <> concat field_subpat_clauses
            )

        pattern_to_clauses _ (SIR.Pattern'NamedADTVariant _ _ _ _ _ _) = todo
        pattern_to_clauses _ (SIR.Pattern'Poison _ _) = pure []

convert_expr (SIR.Expr'Poison id ty sp) = pure $ RIR.Expr'Poison id ty sp
convert_expr (SIR.Expr'Hole id ty sp _) = pure $ RIR.Expr'Poison id ty sp
convert_expr (SIR.Expr'TypeAnnotation _ _ _ _ other) = convert_expr other
convert_expr (SIR.Expr'Forall id ty sp vars e) = RIR.Expr'Forall id sp vars <$> convert_expr e
convert_expr (SIR.Expr'TypeApply id ty sp e (arg, arg_ty)) = RIR.Expr'TypeApply id ty sp <$> convert_expr e <*> pure arg_ty

assign_pattern :: Span -> SIR.Pattern LastSIR -> RIR.Expr -> ConvertState [RIR.Binding]
assign_pattern incomplete_err_sp pat expr = do
    (adt_arena, type_synonym_arena) <- ask
    case PatternCheck.check_complete adt_arena type_synonym_arena incomplete_err_sp [pat] of
        Right () -> pure ()
        Left err -> lift $ lift $ lift $ lift $ Compiler.tell_error (CompletenessError err) >> pure ()

    go pat expr
    where
        go (SIR.Pattern'Variable _ _ var) expr = pure [RIR.Binding var expr]
        go (SIR.Pattern'Wildcard _ _) _ = pure []
        go (SIR.Pattern'Tuple whole_ty whole_sp a b) expr =
            let a_sp = SIR.pattern_span a
                b_sp = SIR.pattern_span b
                a_ty = SIR.pattern_type a
                b_ty = SIR.pattern_type b
            in
            --     (..., ...) = e
            -- becomes
            --     whole = e
            --     ... = case { [whole -> (a, _)] -> a }
            --     ... = case { [whole -> (_, b)] -> b }

            new_variable whole_ty whole_sp >>= \ whole_var ->
            new_variable a_ty a_sp >>= \ a_var ->
            new_variable b_ty b_sp >>= \ b_var ->

            new_made_up_expr_id identity >>= \ l_extract_id ->
            new_made_up_expr_id identity >>= \ r_extract_id ->

            new_made_up_expr_id (\ id -> RIR.Expr'Match id a_ty a_sp (RIR.MatchTree [([RIR.MatchClause'Match whole_var RIR.Match'Tuple, RIR.MatchClause'Assign a_var (RIR.MatchAssignRHS'TupleDestructure1 (SIR.pattern_type a) whole_var)], Right $ RIR.Expr'Refer l_extract_id a_ty a_sp (Just a_var))])) >>= \ extract_a  ->
            new_made_up_expr_id (\ id -> RIR.Expr'Match id b_ty b_sp (RIR.MatchTree [([RIR.MatchClause'Match whole_var RIR.Match'Tuple, RIR.MatchClause'Assign b_var (RIR.MatchAssignRHS'TupleDestructure2 (SIR.pattern_type b) whole_var)], Right $ RIR.Expr'Refer r_extract_id b_ty b_sp (Just b_var))])) >>= \ extract_b ->

            go a extract_a >>= \ assign_a ->
            go b extract_b >>= \ assign_b ->

            pure (RIR.Binding whole_var expr : assign_a ++ assign_b)

        go (SIR.Pattern'Named ty sp _ var other) expr =
            --      a@... = e
            --  becomes
            --      a = e
            --      ... = a
            new_made_up_expr_id (\ id -> RIR.Expr'Refer id ty sp (Just $ unlocate var)) >>= \ refer ->
            go other refer >>= \ other_assignments ->
            pure (RIR.Binding (unlocate var) expr : other_assignments)

        go (SIR.Pattern'AnonADTVariant ty sp _ variant tyargs fields) expr = todo
        go (SIR.Pattern'NamedADTVariant ty sp _ variant tyargs fields) expr = todo

        go (SIR.Pattern'Poison _ _) _ = pure []

sort_bindings :: [RIR.Binding] -> ConvertState RIR.Bindings
sort_bindings bindings = do
    var_arena <- get
    case TopologicalSort.sort_bindings var_arena bindings of
        Right result -> pure result
        Left (errs, result) -> mapM_ (lift . lift . lift . lift . Compiler.tell_error . HasLoops) errs >> pure result
