{-# LANGUAGE DataKinds #-}
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
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result (TypeExprEvaledKey, TypeExprEvaledAsTypeKey)
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NameMaps as NameResolve.NameMaps
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.Misc.Result (InfixGroupedKey, InfixGroupResult)
import Data.Functor.Const (Const)
import qualified UHF.Data.SIR.ID as SIR.ID
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Refs (DeclRef, ValueRef (..))

type Type = Maybe Type.Type

type DIden = Maybe (DeclRef Type.Type)
type VIden = Maybe ValueRef
type PIden = Maybe Type.ADT.VariantIndex

type LastSIR =
    ( NameResolve.NameMaps.NameContextKey, Const () (), Type.Type, TypeExprEvaledKey, TypeExprEvaledAsTypeKey, Maybe Type.Type, InfixGroupedKey)

type VariableArena = Arena.Arena RIR.Variable RIR.VariableKey

data Error
    = CompletenessError (PatternCheck.CompletenessError LastSIR)
    | HasLoops TopologicalSort.BindingsHaveLoopError

instance Diagnostic.ToError Error where
    to_error (CompletenessError c) = Diagnostic.to_error c
    to_error (HasLoops h) = Diagnostic.to_error h

type ConvertState =
    ReaderT
        ( Arena.Arena (SIR.ADT LastSIR) Type.ADTKey
        , Arena.Arena (SIR.TypeSynonym LastSIR) Type.TypeSynonymKey
        , Map (SIR.ID.ID "DeclIden") (Maybe (DeclRef Type.Type))
        , Map (SIR.ID.ID "ValueIden") (Maybe ValueRef)
        , Map (SIR.ID.ID "VariantIden") (Maybe Type.ADT.VariantIndex)
        , Arena.Arena (Maybe (DeclRef Type.Type)) TypeExprEvaledKey
        , Arena.Arena (Maybe Type.Type) TypeExprEvaledAsTypeKey
        , Arena.Arena (Maybe InfixGroupResult) InfixGroupedKey
        )
        (StateT VariableArena (IDGen.IDGenT ID.VariableID (IDGen.IDGenT ID.ExprID (Compiler.WithDiagnostics Error (PatternCheck.NotUseful LastSIR)))))

get_type_expr_evaled_as_type :: TypeExprEvaledAsTypeKey -> ConvertState Type
get_type_expr_evaled_as_type k = do
    (_, _, _, _, _, _, type_expr_evaled_as_type_arena, _) <- ask
    pure $ Arena.get type_expr_evaled_as_type_arena k

get_value_iden_resolved :: SIR.ID.ID "ValueIden" -> ConvertState (Maybe ValueRef)
get_value_iden_resolved k = do
    (_, _, _, value_idens_resolved, _, _, _, _) <- ask
    pure $ value_idens_resolved Map.! k

get_variant_iden_resolved :: SIR.ID.ID "VariantIden" -> ConvertState (Maybe Type.ADT.VariantIndex)
get_variant_iden_resolved k = do
    (_, _, _, _, variant_idens_resolved, _, _, _) <- ask
    pure $ variant_idens_resolved Map.! k

get_infix_grouped :: InfixGroupedKey -> ConvertState (Maybe InfixGroupResult)
get_infix_grouped k = do
    (_, _, _, _, _, _, _, infix_grouped_arena) <- ask
    pure $ Arena.get infix_grouped_arena k

new_made_up_expr_id :: (ID.ExprID -> a) -> ConvertState a
new_made_up_expr_id make =
    lift (lift $ lift IDGen.gen_id) >>= \ id ->
    pure (make id)

convert ::
    Map (SIR.ID.ID "DeclIden") (Maybe (DeclRef Type.Type)) ->
    Map (SIR.ID.ID "ValueIden") (Maybe ValueRef) ->
    Map (SIR.ID.ID "VariantIden") (Maybe Type.ADT.VariantIndex) ->
    Arena.Arena (Maybe (DeclRef Type.Type)) TypeExprEvaledKey ->
    Arena.Arena (Maybe Type.Type) TypeExprEvaledAsTypeKey ->
    Arena.Arena (Maybe InfixGroupResult) InfixGroupedKey ->
    SIR.SIR LastSIR ->
    Compiler.WithDiagnostics Error (PatternCheck.NotUseful LastSIR) RIR.RIR
convert decl_iden_resolved_arena value_iden_resolved_arena variant_iden_resolved_arena type_expr_evaled_arena type_expr_evaled_as_type_arena infix_grouped_arena (SIR.SIR modules adts type_synonyms quant_vars vars (SIR.CU root_module main_function)) = do
    let vars_converted = Arena.transform (\ (SIR.Variable _ id ty (Located sp _)) -> RIR.Variable id ty sp) vars
    ((adts_converted, type_synonyms_converted, cu), vars_with_new) <-
        IDGen.run_id_gen_t ID.ExprID'RIRGen $
            IDGen.run_id_gen_t ID.VariableID'RIRMadeUp $
                runStateT
                    ( runReaderT
                        ( do
                            adts_converted <- Arena.transformM convert_adt adts
                            type_synonyms_converted <- Arena.transformM convert_type_synonym type_synonyms
                            cu <- convert_root_module main_function (Arena.get modules root_module)
                            pure (adts_converted, type_synonyms_converted, cu)
                        )
                        (adts, type_synonyms, decl_iden_resolved_arena, value_iden_resolved_arena, variant_iden_resolved_arena, type_expr_evaled_arena, type_expr_evaled_as_type_arena, infix_grouped_arena)
                    )
                    vars_converted
    pure (RIR.RIR adts_converted type_synonyms_converted quant_vars vars_with_new cu)

convert_root_module :: Maybe SIR.VariableKey -> SIR.Module LastSIR -> ConvertState RIR.CU
convert_root_module main_function (SIR.Module _ _ _ bindings adts type_synonyms) = do
    adt_constructors <- adts
        & mapM ( \ adt_key -> do
            (adts, _, _, _, _, _, _, _) <- ask
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
    (adts, _, _, _, _, _, _, _) <- ask
    let Type.ADT _ _ adt_quant_vars _ = Arena.get adts adt_key
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

    lambdas <- mapM (get_type_expr_evaled_as_type . snd) (Type.ADT.variant_field_types variant) >>= make_lambdas adt_quant_vars variant_index [] >>= wrap_in_forall

    var_arena <- lift get
    var_key <- new_variable (RIR.expr_type var_arena lambdas) variant_name_sp
    pure (var_key, RIR.Binding var_key lambdas)

convert_adt :: SIR.ADT LastSIR -> ConvertState (Type.ADT Type)
convert_adt (Type.ADT id name quant_vars variants) = Type.ADT id name quant_vars <$> mapM convert_variant variants
    where
        convert_variant (Type.ADT.Variant'Named name id fields) = Type.ADT.Variant'Named name id <$> mapM (\ (id, name, (_, ty)) -> (id, name,) <$> get_type_expr_evaled_as_type ty) fields
        convert_variant (Type.ADT.Variant'Anon name id fields) = Type.ADT.Variant'Anon name id <$> mapM (\ (id, (_, ty)) -> (id,) <$> get_type_expr_evaled_as_type ty) fields

convert_type_synonym :: SIR.TypeSynonym LastSIR -> ConvertState (Type.TypeSynonym Type)
convert_type_synonym (Type.TypeSynonym id name (_, expansion)) = Type.TypeSynonym id name <$> get_type_expr_evaled_as_type expansion

convert_binding :: SIR.Binding LastSIR -> ReaderT (Map Type.ADT.VariantIndex RIR.VariableKey) ConvertState [RIR.Binding]
convert_binding (SIR.Binding _ pat eq_sp expr) = convert_expr expr >>= lift . assign_pattern eq_sp pat

new_made_up_var_id :: ConvertState ID.VariableID
new_made_up_var_id = lift $ lift IDGen.gen_id
new_variable :: Type -> Span -> ConvertState SIR.VariableKey
new_variable ty sp =
    new_made_up_var_id >>= \ id ->
    lift (state $ Arena.put (RIR.Variable id ty sp))

convert_expr :: SIR.Expr LastSIR -> ReaderT (Map Type.ADT.VariantIndex RIR.VariableKey) ConvertState RIR.Expr
convert_expr (SIR.Expr'Refer _ id ty sp iden) = do
    adt_constructor_map <- ask
    lift (get_value_iden_resolved (SIR.split_identifier_id iden)) >>= \case
        Just (ValueRef'Variable var) -> pure $ RIR.Expr'Refer id ty sp (Just var)
        Just (ValueRef'ADTVariantConstructor variant) -> pure $ RIR.Expr'Refer id ty sp (Just $ adt_constructor_map Map.! variant) -- TODO: lower these on demand to really ensure that this cannot be partial?
        Just (ValueRef'Intrinsic i) -> pure $ RIR.Expr'Intrinsic id ty sp i
        Nothing -> pure $ RIR.Expr'Refer id ty sp Nothing
convert_expr (SIR.Expr'Char _ id ty sp c) = pure $ RIR.Expr'Char id sp c
convert_expr (SIR.Expr'String _ id ty sp s) = pure $ RIR.Expr'String id sp s
convert_expr (SIR.Expr'Int _ id ty sp i) = pure $ RIR.Expr'Int id sp i
convert_expr (SIR.Expr'Float _ id ty sp f) = pure $ RIR.Expr'Float id sp f
convert_expr (SIR.Expr'Bool _ id ty sp b) = pure $ RIR.Expr'Bool id sp b
convert_expr (SIR.Expr'Tuple _ id ty sp a b) = RIR.Expr'Tuple id sp <$> convert_expr a <*> convert_expr b
convert_expr (SIR.Expr'Lambda _ id ty sp param_pat body) =
    let param_ty = SIR.pattern_type param_pat
        body_sp = SIR.expr_span body
    in
    -- '\ (...) -> body' becomes '\ (arg) -> let ... = arg; body'
    lift (new_variable param_ty (SIR.pattern_span param_pat)) >>= \ param_bk ->
    lift (assign_pattern (SIR.pattern_span param_pat) param_pat (RIR.Expr'Refer id param_ty (SIR.pattern_span param_pat) (Just param_bk))) >>= \ bindings ->
    RIR.Expr'Let id body_sp <$> lift (sort_bindings bindings) <*> pure [] <*> pure [] <*> convert_expr body >>= \ body ->
    pure (RIR.Expr'Lambda id sp param_bk (TopologicalSort.get_captures param_bk body) body)

convert_expr (SIR.Expr'Let _ id ty sp _ bindings adts type_synonyms body) = RIR.Expr'Let id sp <$> (concat <$> mapM convert_binding bindings >>= lift . sort_bindings) <*> pure adts <*> pure type_synonyms <*> convert_expr body -- TODO: define adt constructors for these
convert_expr (SIR.Expr'LetRec _ id ty sp _ bindings adts type_synonyms body) = RIR.Expr'Let id sp <$> (concat <$> mapM convert_binding bindings >>= lift . sort_bindings) <*> pure adts <*> pure type_synonyms <*> convert_expr body -- TODO: define adt constructors for these
convert_expr (SIR.Expr'BinaryOps _ _ void _ _ _ _) = todo
convert_expr (SIR.Expr'Call _ id ty sp callee arg) = RIR.Expr'Call id sp <$> convert_expr callee <*> convert_expr arg
convert_expr (SIR.Expr'If _ id ty sp _ cond true false) =
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

convert_expr (SIR.Expr'Match _ id ty sp match_tok_sp scrutinee arms) = do
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

    (adt_arena, type_synonym_arena, _, _, variant_iden_resolved_arena, _, type_expr_evaled_as_type_arena, _) <- lift ask
    case PatternCheck.check_complete adt_arena type_synonym_arena variant_iden_resolved_arena type_expr_evaled_as_type_arena match_tok_sp (map (\ (_, p, _) -> p) arms) of
        Right () -> pure ()
        Left err -> lift (lift $ lift $ lift $ lift $ Compiler.tell_error $ CompletenessError err) >> pure ()
    case PatternCheck.check_useful adt_arena type_synonym_arena variant_iden_resolved_arena type_expr_evaled_as_type_arena (map (\ (_, p, _) -> p) arms) of
        Right () -> pure ()
        Left warns -> lift $ lift $ lift $ lift $ lift $ Compiler.tell_warnings warns

    arms <- arms
        & mapM (\ (_, pat, result) ->
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
        pattern_to_clauses scrutinee_var (SIR.Pattern'Variable _ _ _ var_key) = pure [RIR.MatchClause'Assign var_key (RIR.MatchAssignRHS'OtherVar scrutinee_var)]
        pattern_to_clauses _ (SIR.Pattern'Wildcard _ _ _) = pure []
        pattern_to_clauses scrutinee_var (SIR.Pattern'Tuple _ _ _ a b) =
            -- scrutinee -> (A, B) becomes [scrutinee -> (,), a = scrutinee.tuple_l, b = scrutinee.tuple_r, a -> A, b -> B]
            new_variable (SIR.pattern_type a) (SIR.pattern_span a) >>= \ a_var ->
            new_variable (SIR.pattern_type b) (SIR.pattern_span b) >>= \ b_var ->

            pattern_to_clauses a_var a >>= \ a_subpat_matchers ->
            pattern_to_clauses b_var b >>= \ b_subpat_matchers ->

            pure (RIR.MatchClause'Match scrutinee_var RIR.Match'Tuple : RIR.MatchClause'Assign a_var (RIR.MatchAssignRHS'TupleDestructure1 (SIR.pattern_type a) scrutinee_var) : RIR.MatchClause'Assign b_var (RIR.MatchAssignRHS'TupleDestructure2 (SIR.pattern_type b) scrutinee_var) : (a_subpat_matchers <> b_subpat_matchers))

        pattern_to_clauses scrutinee_var (SIR.Pattern'Named _ _ _ _ (Located _ var_key) subpat) =
            -- scrutinee -> name@P becomes [name = scrutinee, scrutinee -> P]
            pattern_to_clauses scrutinee_var subpat >>= \ subpat_matchers ->
            pure (RIR.MatchClause'Assign var_key (RIR.MatchAssignRHS'OtherVar scrutinee_var) : subpat_matchers)

        pattern_to_clauses scrutinee_var (SIR.Pattern'AnonADTVariant _ ty sp variant_iden tyargs fields) = do
            -- Variant(F0, F1, ...) becomes [scrutinee -> Variant, f0 = (scrutinee as Variant).0, f1 = (scrutinee as Variant).1, f0 -> F0, f1 -> F1, ...]
            field_vars <- fields & mapM (\ pat -> new_variable (SIR.pattern_type pat) (SIR.pattern_span pat))
            field_subpat_clauses <- zipWithM pattern_to_clauses field_vars fields
            (adt_arena, _, _, _, _, _, _, _) <- ask
            variant_index <- get_variant_iden_resolved $ SIR.split_identifier_id variant_iden
            let field_idxs = Type.ADT.variant_field_idxs adt_arena <$> variant_index
            pure $
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

        pattern_to_clauses _ (SIR.Pattern'NamedADTVariant _ _ _ _ _ _) = todo
        pattern_to_clauses _ (SIR.Pattern'Poison _ _ _) = pure []

convert_expr (SIR.Expr'Poison _ id ty sp) = pure $ RIR.Expr'Poison id ty sp
convert_expr (SIR.Expr'Hole _ id ty sp _) = pure $ RIR.Expr'Poison id ty sp
convert_expr (SIR.Expr'TypeAnnotation _ _ _ _ _ other) = convert_expr other
convert_expr (SIR.Expr'Forall _ id ty sp _ vars e) = RIR.Expr'Forall id sp vars <$> convert_expr e
convert_expr (SIR.Expr'TypeApply _ id ty sp e (arg, arg_ty)) = RIR.Expr'TypeApply id ty sp <$> convert_expr e <*> lift (get_type_expr_evaled_as_type arg_ty)

assign_pattern :: Span -> SIR.Pattern LastSIR -> RIR.Expr -> ConvertState [RIR.Binding]
assign_pattern incomplete_err_sp pat expr = do
    (adt_arena, type_synonym_arena, _, _, variant_iden_resolved_arena, _, type_expr_evaled_as_type_arena, _) <- ask
    case PatternCheck.check_complete adt_arena type_synonym_arena variant_iden_resolved_arena type_expr_evaled_as_type_arena incomplete_err_sp [pat] of
        Right () -> pure ()
        Left err -> lift $ lift $ lift $ lift $ Compiler.tell_error (CompletenessError err) >> pure ()

    go pat expr
    where
        go (SIR.Pattern'Variable _ _ _ var) expr = pure [RIR.Binding var expr]
        go (SIR.Pattern'Wildcard _ _ _) _ = pure []
        go (SIR.Pattern'Tuple _ whole_ty whole_sp a b) expr =
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

        go (SIR.Pattern'Named _ ty sp _ var other) expr =
            --      a@... = e
            --  becomes
            --      a = e
            --      ... = a
            new_made_up_expr_id (\ id -> RIR.Expr'Refer id ty sp (Just $ unlocate var)) >>= \ refer ->
            go other refer >>= \ other_assignments ->
            pure (RIR.Binding (unlocate var) expr : other_assignments)

        go (SIR.Pattern'AnonADTVariant _ ty sp variant tyargs fields) expr = todo
        go (SIR.Pattern'NamedADTVariant _ ty sp variant tyargs fields) expr = todo

        go (SIR.Pattern'Poison _ _ _) _ = pure []

sort_bindings :: [RIR.Binding] -> ConvertState RIR.Bindings
sort_bindings bindings = do
    var_arena <- get
    case TopologicalSort.sort_bindings var_arena bindings of
        Right result -> pure result
        Left (errs, result) -> mapM_ (lift . lift . lift . lift . Compiler.tell_error . HasLoops) errs >> pure result
