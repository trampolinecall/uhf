module UHF.Phases.ToRIR (convert) where

import UHF.Util.Prelude

import qualified Arena
import qualified Unique

import UHF.IO.Located (Located (Located, unlocate))

import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.RIR as RIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.IR.IDGen as IDGen

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

type ConvertState = Unique.UniqueMakerT (ReaderT (Arena.Arena (Type.ADT Type) Type.ADTKey) (StateT BoundValueArena (IDGen.IDGenT ID.BoundValueID (IDGen.IDGen ID.ExprID))))

new_made_up_expr_id :: (ID.ExprID -> a) -> ConvertState a
new_made_up_expr_id make =
    (lift $ lift $ lift $ lift IDGen.gen_id) >>= \ id ->
    pure (make id)

convert :: SIR -> RIR.RIR
convert (SIR.SIR _ modules adts type_synonyms type_vars bvs mod) =
    let adts_converted = Arena.transform convert_adt adts
        type_synonyms_converted = Arena.transform convert_type_synonym type_synonyms
        bvs_converted =
            Arena.transform
                (\case
                    SIR.BoundValue id ty (Located sp _) -> RIR.BoundValue id ty (RIR.JustSpan sp)
                    SIR.BoundValue'ADTVariant id _ _ ty sp -> RIR.BoundValue id ty (RIR.JustSpan sp)
                )
                bvs
        (cu, bvs_with_new) = IDGen.run_id_gen ID.ExprID'RIRGen $ IDGen.run_id_gen_t ID.BoundValueID'RIRMadeUp $ runStateT (runReaderT (Unique.run_unique_maker_t (assemble_cu modules mod)) adts_converted) bvs_converted
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
convert_binding (SIR.Binding'ADTVariant _ bvk type_params variant_index) =
    lift ask >>= \ adts ->
    let variant = Type.get_adt_variant adts variant_index

        wrap_in_forall = case type_params of
            [] -> pure
            param:more -> \ lambda -> new_made_up_expr_id (\ id -> RIR.Expr'Forall id (Type.Type'Forall (param :| more) <$> RIR.expr_type lambda) RIR.Desugared (param :| more) lambda)
    in make_lambdas type_params variant_index [] (Type.variant_field_types variant) >>= wrap_in_forall >>= \ lambdas ->
    pure [RIR.Binding bvk lambdas]
    where
        make_lambdas type_params variant_index@(Type.ADTVariantIndex adt_key _) refer_to_params [] =
            let ty_params_as_tys = map Type.Type'Variable type_params
            in new_made_up_expr_id $ \ id -> RIR.Expr'MakeADT id (Type.Type'ADT adt_key ty_params_as_tys) RIR.Desugared variant_index (map Just ty_params_as_tys) refer_to_params

        make_lambdas type_params variant_index refer_to_params (cur_field_ty:more_field_tys) =
            Unique.make_unique >>= \ lambda_uniq ->
            new_bound_value cur_field_ty RIR.Desugared >>= \ param_bvk ->
            new_made_up_expr_id (\ id -> RIR.Expr'Identifier id cur_field_ty RIR.Desugared (Just param_bvk)) >>= \ refer_expr ->

            make_lambdas type_params variant_index (refer_to_params <> [refer_expr]) more_field_tys >>= \ lambda_result ->
            let lambda_ty = Type.Type'Function <$> cur_field_ty <*> RIR.expr_type lambda_result
            in new_made_up_expr_id (\ id -> RIR.Expr'Lambda id lambda_ty RIR.Desugared lambda_uniq param_bvk lambda_result)

new_made_up_bv_id :: ConvertState ID.BoundValueID
new_made_up_bv_id = lift $ lift $ lift IDGen.gen_id
new_bound_value :: Type -> RIR.MaybeSpan -> ConvertState SIR.BoundValueKey
new_bound_value ty sp =
    new_made_up_bv_id >>= \ id ->
    lift (lift $ state $ Arena.put (RIR.BoundValue id ty sp))

convert_expr :: SIRExpr -> ConvertState RIRExpr
convert_expr (SIR.Expr'Identifier id ty sp bv) = pure $ RIR.Expr'Identifier id ty (RIR.JustSpan sp) (unlocate bv)
convert_expr (SIR.Expr'Char id ty sp c) = pure $ RIR.Expr'Char id ty (RIR.JustSpan sp) c
convert_expr (SIR.Expr'String id ty sp s) = pure $ RIR.Expr'String id ty (RIR.JustSpan sp) s
convert_expr (SIR.Expr'Int id ty sp i) = pure $ RIR.Expr'Int id ty (RIR.JustSpan sp) i
convert_expr (SIR.Expr'Float id ty sp f) = pure $ RIR.Expr'Float id ty (RIR.JustSpan sp) f
convert_expr (SIR.Expr'Bool id ty sp b) = pure $ RIR.Expr'Bool id ty (RIR.JustSpan sp) b
convert_expr (SIR.Expr'Tuple id ty sp a b) = RIR.Expr'Tuple id ty (RIR.JustSpan sp) <$> convert_expr a <*> convert_expr b
convert_expr (SIR.Expr'Lambda id ty sp param_pat body) =
    let param_ty = SIR.pattern_type param_pat
        body_ty = SIR.expr_type body
    in
    -- '\ (...) -> body' becomes '\ (arg) -> let ... = arg; body'
    new_bound_value param_ty RIR.Desugared >>= \ param_bk ->
    assign_pattern param_pat (RIR.Expr'Identifier id param_ty (RIR.Desugared) (Just param_bk)) >>= \ bindings ->
    Unique.make_unique >>= \ uniq -> -- TODO: remove?
    RIR.Expr'Lambda id ty (RIR.JustSpan sp) uniq param_bk <$> (RIR.Expr'Let id body_ty (RIR.Desugared) bindings <$> convert_expr body)

convert_expr (SIR.Expr'Let id ty sp bindings body) = RIR.Expr'Let id ty (RIR.JustSpan sp) <$> (concat <$> mapM convert_binding bindings) <*> convert_expr body
convert_expr (SIR.Expr'LetRec id ty sp bindings body) = RIR.Expr'Let id ty (RIR.JustSpan sp) <$> (concat <$> mapM convert_binding bindings) <*> convert_expr body
convert_expr (SIR.Expr'BinaryOps _ void _ _ _ _) = absurd void
convert_expr (SIR.Expr'Call id ty sp callee arg) = RIR.Expr'Call id ty (RIR.JustSpan sp) <$> convert_expr callee <*> convert_expr arg
convert_expr (SIR.Expr'If id ty sp _ cond true false) = RIR.Expr'Switch id ty (RIR.JustSpan sp) <$> convert_expr cond <*> sequence [(,) (RIR.Switch'BoolLiteral True) <$> convert_expr true, (,) (RIR.Switch'BoolLiteral False) <$> convert_expr false]
convert_expr (SIR.Expr'Case _ _ _ _ _ _) = todo -- TODO: case desguaring RIR.Expr'Switch id ty sp <$> convert_expr expr <*> mapM (\ (pat, expr) -> (,) <$> convert_pattern pat <*> convert_expr expr) arms
convert_expr (SIR.Expr'Poison id ty sp) = pure $ RIR.Expr'Poison id ty (RIR.JustSpan sp)
convert_expr (SIR.Expr'Hole id ty sp _) = pure $ RIR.Expr'Poison id ty (RIR.JustSpan sp)
convert_expr (SIR.Expr'TypeAnnotation _ _ _ _ other) = convert_expr other
convert_expr (SIR.Expr'Forall id ty sp vars e) = RIR.Expr'Forall id ty (RIR.JustSpan sp) vars <$> convert_expr e
convert_expr (SIR.Expr'TypeApply id ty sp e arg) = RIR.Expr'TypeApply id ty (RIR.JustSpan sp) <$> convert_expr e <*> pure (SIR.type_expr_type_info arg)

assign_pattern :: SIRPattern -> RIRExpr -> ConvertState [RIRBinding]
assign_pattern (SIR.Pattern'Identifier _ _ bv) expr = pure [RIR.Binding bv expr]
assign_pattern (SIR.Pattern'Wildcard _ _) _ = pure []
assign_pattern (SIR.Pattern'Tuple whole_ty _ a b) expr =
    let a_ty = SIR.pattern_type a
        b_ty = SIR.pattern_type b
    in
    --     (..., ...) = e
    -- becomes
    --     whole = e
    --     ... = case whole { (a, _) -> a }
    --     ... = case whole { (_, b) -> b }

    new_bound_value whole_ty RIR.Desugared >>= \ whole_bv ->
    new_bound_value a_ty RIR.Desugared >>= \ a_bv ->
    new_bound_value b_ty RIR.Desugared >>= \ b_bv ->

    new_made_up_expr_id identity >>= \ l_extract_id ->
    new_made_up_expr_id identity >>= \ r_extract_id ->

    new_made_up_expr_id (\ id -> RIR.Expr'Identifier id whole_ty RIR.Desugared (Just whole_bv)) >>= \ l_whole_expr ->
    new_made_up_expr_id (\ id -> RIR.Expr'Identifier id whole_ty RIR.Desugared (Just whole_bv)) >>= \ r_whole_expr  ->
    new_made_up_expr_id (\ id -> RIR.Expr'Switch id a_ty RIR.Desugared l_whole_expr [(RIR.Switch'Tuple (Just a_bv) Nothing, RIR.Expr'Identifier l_extract_id a_ty RIR.Desugared (Just a_bv))]) >>= \ extract_a  ->
    new_made_up_expr_id (\ id -> RIR.Expr'Switch id b_ty RIR.Desugared r_whole_expr [(RIR.Switch'Tuple Nothing (Just b_bv), RIR.Expr'Identifier r_extract_id b_ty RIR.Desugared (Just b_bv))]) >>= \ extract_b ->

    assign_pattern a extract_a >>= \ assign_a ->
    assign_pattern b extract_b >>= \ assign_b ->

    pure (RIR.Binding whole_bv expr : assign_a ++ assign_b)

assign_pattern (SIR.Pattern'Named ty _ _ bv other) expr =
    --      a@... = e
    --  becomes
    --      a = e
    --      ... = a
    new_made_up_expr_id (\ id -> RIR.Expr'Identifier id ty RIR.Desugared (Just $ unlocate bv)) >>= \ refer ->
    assign_pattern other refer >>= \ other_assignments ->
    pure (RIR.Binding (unlocate bv) expr : other_assignments)

assign_pattern (SIR.Pattern'AnonADTVariant ty sp variant fields) expr = todo
assign_pattern (SIR.Pattern'NamedADTVariant ty sp variant fields) expr = todo

assign_pattern (SIR.Pattern'Poison _ _) _ = pure []
