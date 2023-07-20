module UHF.Phases.ToRIR (convert) where

import UHF.Util.Prelude

import qualified Arena

import UHF.IO.Span (Span)
import UHF.IO.Located (Located (Located, unlocate))

import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.RIR as RIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.IR.IDGen as IDGen

import qualified Data.List as List

type Type = Maybe (Type.Type Void)

type DIden = Maybe SIR.DeclKey
type VIden = Located (Maybe SIR.BoundValueKey)
type PIden = Maybe Type.ADTVariantIndex
type SIR = SIR.SIR DIden VIden PIden Type Void
type SIRModule = SIR.Module DIden VIden PIden Type Void
type SIRExpr = SIR.Expr DIden VIden PIden Type Void
type SIRTypeExpr = SIR.TypeExpr DIden Type
type SIRPattern = SIR.Pattern PIden Type
type SIRBinding = SIR.Binding DIden VIden PIden Type Void

type RIRExpr = RIR.Expr
type RIRBinding = RIR.Binding

type BoundValueArena = Arena.Arena RIR.BoundValue RIR.BoundValueKey

type ConvertState = ReaderT (Arena.Arena (Type.ADT Type) Type.ADTKey) (StateT BoundValueArena (IDGen.IDGenT ID.BoundValueID (IDGen.IDGen ID.ExprID)))

new_made_up_expr_id :: (ID.ExprID -> a) -> ConvertState a
new_made_up_expr_id make =
    (lift $ lift $ lift IDGen.gen_id) >>= \ id ->
    pure (make id)

convert :: SIR -> RIR.RIR
convert (SIR.SIR _ modules adts type_synonyms type_vars bvs mod) =
    let adts_converted = Arena.transform convert_adt adts
        type_synonyms_converted = Arena.transform convert_type_synonym type_synonyms
        bvs_converted =
            Arena.transform
                (\case
                    SIR.BoundValue id ty (Located sp _) -> RIR.BoundValue id ty sp
                    SIR.BoundValue'ADTVariant id _ _ ty sp -> RIR.BoundValue id ty sp
                )
                bvs
        (cu, bvs_with_new) = IDGen.run_id_gen ID.ExprID'RIRGen $ IDGen.run_id_gen_t ID.BoundValueID'RIRMadeUp $ runStateT (runReaderT (assemble_cu modules mod) adts_converted) bvs_converted
    in RIR.RIR adts_converted type_synonyms_converted type_vars bvs_with_new cu

assemble_cu :: Arena.Arena SIRModule SIR.ModuleKey -> SIR.ModuleKey -> ConvertState RIR.CU
assemble_cu modules mod =
    let SIR.Module _ bindings adts syns = Arena.get modules mod
    in RIR.CU <$> (concat <$> mapM convert_binding bindings) <*> pure adts <*> pure syns

convert_adt :: Type.ADT SIRTypeExpr -> Type.ADT Type
convert_adt (Type.ADT id name type_vars variants) = Type.ADT id name type_vars (map convert_variant variants)
    where
        convert_variant (Type.ADTVariant'Named name fields) = Type.ADTVariant'Named name (map (\ (name, ty) -> (name, SIR.type_expr_type_info ty)) fields)
        convert_variant (Type.ADTVariant'Anon name fields) = Type.ADTVariant'Anon name (map SIR.type_expr_type_info fields)

convert_type_synonym :: Type.TypeSynonym SIRTypeExpr -> Type.TypeSynonym Type
convert_type_synonym (Type.TypeSynonym id name expansion) = Type.TypeSynonym id name (SIR.type_expr_type_info expansion)

convert_binding :: SIRBinding -> ConvertState [RIRBinding]
convert_binding (SIR.Binding pat _ expr) = convert_expr expr >>= assign_pattern pat
convert_binding (SIR.Binding'ADTVariant name_sp bvk type_params variant_index) =
    ask >>= \ adts ->
    let variant = Type.get_adt_variant adts variant_index

        wrap_in_forall = case type_params of
            [] -> pure
            param:more -> \ lambda -> new_made_up_expr_id (\ id -> RIR.Expr'Forall id name_sp (param :| more) lambda)
    in
    make_lambdas type_params variant_index [] (Type.variant_field_types variant) >>= wrap_in_forall >>= \ lambdas ->
    pure [RIR.Binding bvk lambdas]
    where
        make_lambdas type_params variant_index refer_to_params [] =
            let ty_params_as_tys = map Type.Type'Variable type_params
            in new_made_up_expr_id $ \ id -> RIR.Expr'MakeADT id name_sp variant_index (map Just ty_params_as_tys) refer_to_params

        make_lambdas type_params variant_index refer_to_params (cur_field_ty:more_field_tys) =
            new_bound_value cur_field_ty name_sp >>= \ param_bvk ->
            new_made_up_expr_id (\ id -> RIR.Expr'Identifier id name_sp (Just param_bvk)) >>= \ refer_expr ->

            make_lambdas type_params variant_index (refer_to_params <> [refer_expr]) more_field_tys >>= \ lambda_result ->
            lift get >>= \ bv_arena ->
            new_made_up_expr_id (\ id -> RIR.Expr'Lambda id name_sp param_bvk lambda_result)

new_made_up_bv_id :: ConvertState ID.BoundValueID
new_made_up_bv_id = lift $ lift IDGen.gen_id
new_bound_value :: Type -> Span -> ConvertState SIR.BoundValueKey
new_bound_value ty sp =
    new_made_up_bv_id >>= \ id ->
    lift (state $ Arena.put (RIR.BoundValue id ty sp))

convert_expr :: SIRExpr -> ConvertState RIRExpr
convert_expr (SIR.Expr'Identifier id ty sp bv) = pure $ RIR.Expr'Identifier id sp (unlocate bv)
convert_expr (SIR.Expr'Char id ty sp c) = pure $ RIR.Expr'Char id sp c
convert_expr (SIR.Expr'String id ty sp s) = pure $ RIR.Expr'String id sp s
convert_expr (SIR.Expr'Int id ty sp i) = pure $ RIR.Expr'Int id sp i
convert_expr (SIR.Expr'Float id ty sp f) = pure $ RIR.Expr'Float id sp f
convert_expr (SIR.Expr'Bool id ty sp b) = pure $ RIR.Expr'Bool id sp b
convert_expr (SIR.Expr'Tuple id ty sp a b) = RIR.Expr'Tuple id sp <$> convert_expr a <*> convert_expr b
convert_expr (SIR.Expr'Lambda id ty sp param_pat body) =
    let param_ty = SIR.pattern_type param_pat
        body_ty = SIR.expr_type body
        body_sp = SIR.expr_span body
    in
    -- '\ (...) -> body' becomes '\ (arg) -> let ... = arg; body'
    new_bound_value param_ty (SIR.pattern_span param_pat) >>= \ param_bk ->
    assign_pattern param_pat (RIR.Expr'Identifier id (SIR.pattern_span param_pat) (Just param_bk)) >>= \ bindings ->
    RIR.Expr'Lambda id sp param_bk <$> (RIR.Expr'Let id body_sp bindings <$> convert_expr body)

convert_expr (SIR.Expr'Let id ty sp bindings body) = RIR.Expr'Let id sp <$> (concat <$> mapM convert_binding bindings) <*> convert_expr body
convert_expr (SIR.Expr'LetRec id ty sp bindings body) = RIR.Expr'Let id sp <$> (concat <$> mapM convert_binding bindings) <*> convert_expr body
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

    lift get >>= \ bv_arena ->
    new_bound_value (RIR.expr_type bv_arena cond) (RIR.expr_span cond) >>= \ cond_bv ->

    new_made_up_expr_id
        (\ let_id ->
            RIR.Expr'Let let_id sp
                [RIR.Binding cond_bv cond]
                (RIR.Expr'Case id ty sp
                    (RIR.CaseTree
                        [ ([RIR.CaseClause'Match cond_bv (RIR.Case'BoolLiteral True)], Right true)
                        , ([RIR.CaseClause'Match cond_bv (RIR.Case'BoolLiteral False)], Right false)
                        ]
                    )
                )
        )

convert_expr (SIR.Expr'Case id ty sp _ scrutinee arms) =
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

    convert_expr scrutinee >>= \ scrutinee ->
    lift get >>= \ bv_arena ->
    new_bound_value (RIR.expr_type bv_arena scrutinee) (RIR.expr_span scrutinee) >>= \ scrutinee_bv ->

    -- TODO: exhaustiveness check and unreachable patterns check
    mapM
        (\ (pat, result) ->
            pattern_to_clauses scrutinee_bv pat >>= \ clauses ->
            convert_expr result >>= \ result ->
            pure (clauses, Right result)
        )
        arms >>= \ arms ->

    new_made_up_expr_id
        (\ let_id ->
            RIR.Expr'Let let_id sp
                [RIR.Binding scrutinee_bv scrutinee]
                (RIR.Expr'Case id ty sp (RIR.CaseTree arms))
        )
    where
        pattern_to_clauses scrutinee_bv (SIR.Pattern'Identifier _ _ bvk) = pure [RIR.CaseClause'Assign bvk (RIR.CaseAssignRHS'OtherBVK scrutinee_bv)]
        pattern_to_clauses _ (SIR.Pattern'Wildcard _ _) = pure []
        pattern_to_clauses scrutinee_bv (SIR.Pattern'Tuple _ _ a b) =
            -- scrutinee -> (A, B) becomes [scrutinee -> (,), a = scrutinee.tuple_l, b = scrutinee.tuple_r, a -> A, b -> B]
            new_bound_value (SIR.pattern_type a) (SIR.pattern_span a) >>= \ a_bv ->
            new_bound_value (SIR.pattern_type b) (SIR.pattern_span b) >>= \ b_bv ->

            pattern_to_clauses a_bv a >>= \ a_subpat_matchers ->
            pattern_to_clauses b_bv b >>= \ b_subpat_matchers ->

            pure (RIR.CaseClause'Match scrutinee_bv RIR.Case'Tuple : RIR.CaseClause'Assign a_bv (RIR.CaseAssignRHS'TupleDestructure1 (SIR.pattern_type a) scrutinee_bv) : RIR.CaseClause'Assign b_bv (RIR.CaseAssignRHS'TupleDestructure2 (SIR.pattern_type b) scrutinee_bv) : (a_subpat_matchers <> b_subpat_matchers))

        pattern_to_clauses scrutinee_bv (SIR.Pattern'Named _ _ _ (Located _ bvk) subpat) =
            -- scrutinee -> name@P becomes [name = scrutinee, scrutinee -> P]
            pattern_to_clauses scrutinee_bv subpat >>= \ subpat_matchers ->
            pure (RIR.CaseClause'Assign bvk (RIR.CaseAssignRHS'OtherBVK scrutinee_bv) : subpat_matchers)

        pattern_to_clauses scrutinee_bv (SIR.Pattern'AnonADTVariant ty sp variant_index tyargs fields) =
            -- Variant(F0, F1, ...) becomes [scrutinee -> Variant, f0 = (scrutinee as Variant).0, f1 = (scrutinee as Variant).1, f0 -> F0, f1 -> F1, ...]
            fields & mapM (\ pat -> new_bound_value (SIR.pattern_type pat) (SIR.pattern_span pat)) >>= \ field_bvs ->
            zipWithM pattern_to_clauses field_bvs fields >>= \ field_subpat_clauses ->
            pure (RIR.CaseClause'Match scrutinee_bv (RIR.Case'AnonADTVariant variant_index) : zipWith (\ i field_bv -> RIR.CaseClause'Assign field_bv (RIR.CaseAssignRHS'AnonADTVariantField (SIR.pattern_type $ fields List.!! i) scrutinee_bv variant_index i)) [0..] field_bvs <> concat field_subpat_clauses)

        pattern_to_clauses _ (SIR.Pattern'NamedADTVariant _ _ _ _ _) = todo
        pattern_to_clauses _ (SIR.Pattern'Poison _ _) = pure []

convert_expr (SIR.Expr'Poison id ty sp) = pure $ RIR.Expr'Poison id ty sp
convert_expr (SIR.Expr'Hole id ty sp _) = pure $ RIR.Expr'Poison id ty sp
convert_expr (SIR.Expr'TypeAnnotation _ _ _ _ other) = convert_expr other
convert_expr (SIR.Expr'Forall id ty sp vars e) = RIR.Expr'Forall id sp vars <$> convert_expr e
convert_expr (SIR.Expr'TypeApply id ty sp e arg) = RIR.Expr'TypeApply id ty sp <$> convert_expr e <*> pure (SIR.type_expr_type_info arg)

-- TODO: exhaustiveness check
assign_pattern :: SIRPattern -> RIRExpr -> ConvertState [RIRBinding]
assign_pattern (SIR.Pattern'Identifier _ _ bv) expr = pure [RIR.Binding bv expr]
assign_pattern (SIR.Pattern'Wildcard _ _) _ = pure []
assign_pattern (SIR.Pattern'Tuple whole_ty whole_sp a b) expr =
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

    new_bound_value whole_ty whole_sp >>= \ whole_bv ->
    new_bound_value a_ty a_sp >>= \ a_bv ->
    new_bound_value b_ty b_sp >>= \ b_bv ->

    new_made_up_expr_id identity >>= \ l_extract_id ->
    new_made_up_expr_id identity >>= \ r_extract_id ->

    new_made_up_expr_id (\ id -> RIR.Expr'Case id a_ty a_sp (RIR.CaseTree [([RIR.CaseClause'Match whole_bv RIR.Case'Tuple, RIR.CaseClause'Assign a_bv (RIR.CaseAssignRHS'TupleDestructure1 (SIR.pattern_type a) whole_bv)], Right $ RIR.Expr'Identifier l_extract_id a_sp (Just a_bv))])) >>= \ extract_a  ->
    new_made_up_expr_id (\ id -> RIR.Expr'Case id b_ty b_sp (RIR.CaseTree [([RIR.CaseClause'Match whole_bv RIR.Case'Tuple, RIR.CaseClause'Assign b_bv (RIR.CaseAssignRHS'TupleDestructure2 (SIR.pattern_type b) whole_bv)], Right $ RIR.Expr'Identifier r_extract_id b_sp (Just b_bv))])) >>= \ extract_b ->

    assign_pattern a extract_a >>= \ assign_a ->
    assign_pattern b extract_b >>= \ assign_b ->

    pure (RIR.Binding whole_bv expr : assign_a ++ assign_b)

assign_pattern (SIR.Pattern'Named ty sp _ bv other) expr =
    --      a@... = e
    --  becomes
    --      a = e
    --      ... = a
    new_made_up_expr_id (\ id -> RIR.Expr'Identifier id sp (Just $ unlocate bv)) >>= \ refer ->
    assign_pattern other refer >>= \ other_assignments ->
    pure (RIR.Binding (unlocate bv) expr : other_assignments)

assign_pattern (SIR.Pattern'AnonADTVariant ty sp variant tyargs fields) expr = todo
assign_pattern (SIR.Pattern'NamedADTVariant ty sp variant tyargs fields) expr = todo

assign_pattern (SIR.Pattern'Poison _ _) _ = pure []
