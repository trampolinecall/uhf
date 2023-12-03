module UHF.Phases.ToSIR (convert) where

import UHF.Prelude

import Control.Monad.Trans.Maybe (runMaybeT)

import UHF.Source.Located (Located (..))
import UHF.Source.Span (Span)
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.AST as AST
import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Util.Arena as Arena

data Error
    = Tuple1 Span
    | Tuple0 Span

data DeclAt = DeclAt Span | ImplicitPrim deriving Show

instance Diagnostic.ToError Error where
    to_error (Tuple1 sp) = Diagnostic.Error (Just sp) "tuple of 1 element" [] []
    to_error (Tuple0 sp) = Diagnostic.Error (Just sp) "tuple of 0 elements" [] []

type SIRStage = (Located Text, (), (), Located Text, (), Located Text, (), (), ())

type SIR = SIR.SIR SIRStage

type Module = SIR.Module SIRStage
type Binding = SIR.Binding SIRStage
type ADT = Type.ADT (TypeExpr, ())
type TypeSynonym = Type.TypeSynonym (TypeExpr, ())
type TypeExpr = SIR.TypeExpr SIRStage
type Expr = SIR.Expr SIRStage
type Pattern = SIR.Pattern SIRStage
type Variable = SIR.Variable SIRStage

type ModuleArena = Arena.Arena Module SIR.ModuleKey
type ADTArena = Arena.Arena ADT Type.ADTKey
type TypeSynonymArena = Arena.Arena TypeSynonym Type.TypeSynonymKey
type VariableArena = Arena.Arena Variable SIR.VariableKey
type QuantVarArena = Arena.Arena Type.QuantVar Type.QuantVarKey

type MakeIRState = StateT (ModuleArena, ADTArena, TypeSynonymArena, QuantVarArena, VariableArena) (Compiler.WithDiagnostics Error Void)

new_module :: Module -> MakeIRState SIR.ModuleKey
new_module m =
    state $ \ (mods, adts, type_synonyms, type_vars, variables) ->
        let (key, mods') = Arena.put m mods
        in (key, (mods', adts, type_synonyms, type_vars, variables))

new_adt :: ADT -> MakeIRState Type.ADTKey
new_adt adt =
    state $ \ (mods, adts, type_synonyms, type_vars, variables) ->
        let (key, adts') = Arena.put adt adts
        in (key, (mods, adts', type_synonyms, type_vars, variables))

new_type_synonym :: TypeSynonym -> MakeIRState Type.TypeSynonymKey
new_type_synonym ts =
    state $ \ (mods, adts, type_synonyms, type_vars, variables) ->
        let (key, type_synonyms') = Arena.put ts type_synonyms
        in (key, (mods, adts, type_synonyms', type_vars, variables))

new_type_var :: Located Text -> MakeIRState Type.QuantVarKey
new_type_var name =
    state $ \ (mods, adts, type_synonyms, type_vars, variables) ->
        let (key, type_vars') = Arena.put (Type.QuantVar name) type_vars
        in (key, (mods, adts, type_synonyms, type_vars', variables))

new_variable :: Variable -> MakeIRState SIR.VariableKey
new_variable var =
    state $ \ (mods, adts, type_synonyms, type_vars, variables) ->
        let (key, variables') = Arena.put var variables
        in (key, (mods, adts, type_synonyms, type_vars, variables'))

tell_error :: Error -> MakeIRState ()
tell_error = lift . Compiler.tell_error

convert :: [AST.Decl] -> Compiler.WithDiagnostics Error Void SIR
convert decls =
    runStateT
        (
            let module_id = ID.ModuleID'Root
            in convert_decls (ID.VarParent'Module module_id) (ID.DeclParent'Module module_id) decls >>= \ (bindings, adts, type_synonyms) ->
            new_module (SIR.Module module_id bindings adts type_synonyms)
        )
        (Arena.new, Arena.new, Arena.new, Arena.new, Arena.new) >>= \ (mod, (mods, adts, type_synonyms, type_vars, variables)) ->
    pure (SIR.SIR mods adts type_synonyms type_vars variables mod)

convert_decls :: ID.VariableParent -> ID.DeclParent -> [AST.Decl] -> MakeIRState ([Binding], [Type.ADTKey], [Type.TypeSynonymKey])
convert_decls var_parent decl_parent decls =
    unzip3 <$> zipWithM convert_decl [0..] decls >>= \ (bindings, adts, type_synonyms) ->
    pure (concat bindings, concat adts, concat type_synonyms)
    where
        convert_decl :: Int -> AST.Decl -> MakeIRState ([Binding], [Type.ADTKey], [Type.TypeSynonymKey])
        convert_decl ind (AST.Decl'Value target eq_sp expr) =
            convert_expr (ID.ExprID'InitializerOf decl_parent ind) expr >>= \ expr' ->
            convert_pattern var_parent target >>= \ target' ->
            pure ([SIR.Binding target' eq_sp expr'], [], [])

        convert_decl _ (AST.Decl'Data l_data_name@(Located _ data_name) type_params variants) =
            runMaybeT (
                mapM (lift . new_type_var) type_params >>= \ ty_param_vars ->

                let adt_id = ID.DeclID decl_parent data_name
                in mapM (convert_variant adt_id) variants >>= \ variants_converted ->
                let adt = Type.ADT adt_id l_data_name ty_param_vars variants_converted
                in

                lift (new_adt adt) >>= \ adt_key ->

                -- TODO: use Type.adt_variant_idxs?
                (catMaybes <$> mapM
                    (\ case
                        (Type.ADT.Variant'Anon (Located name_sp name) _ _, index) ->
                            mapM (lift . new_type_var) type_params >>= \ ty_param_vars_for_constructor ->
                            let variant_index = Type.ADT.VariantIndex adt_key index
                            in lift (new_variable (SIR.Variable'ADTVariant (ID.VariableID var_parent name) variant_index ty_param_vars_for_constructor () name_sp)) >>= \ var_key ->
                            pure (Just (SIR.Binding'ADTVariant name_sp var_key ty_param_vars_for_constructor variant_index))
                        (Type.ADT.Variant'Named _ _ _, _) -> pure Nothing
                    )
                    (zip variants_converted [0..])) >>= \ constructor_bindings ->

                pure (adt_key, constructor_bindings)
            ) >>= \case
                Just (adt_key, constructor_bindings) -> pure (constructor_bindings, [adt_key], [])
                Nothing -> pure ([], [], [])

        convert_decl _ (AST.Decl'TypeSyn name expansion) =
            runMaybeT (
                lift (convert_type expansion) >>= \ expansion' ->
                let l_syn_name@(Located _ syn_name) = name -- TODO: remove
                in lift (new_type_synonym (Type.TypeSynonym (ID.DeclID decl_parent syn_name) l_syn_name (expansion', ())))
            ) >>= \case
                Just syn_key -> pure ([], [], [syn_key])
                Nothing -> pure ([], [], [])

        convert_variant adt_id (AST.DataVariant'Anon variant_name fields) =
            let variant_id = ID.ADTVariantID adt_id (unlocate variant_name)
            in Type.ADT.Variant'Anon variant_name variant_id
                <$> zipWithM
                    (\ field_idx ty_ast ->
                        lift (convert_type ty_ast) >>= \ ty ->
                        pure (ID.ADTFieldID variant_id (show (field_idx :: Int)), (ty, ())))
                    [0..]
                    fields
        convert_variant adt_id (AST.DataVariant'Named variant_name fields) =
            let variant_id = ID.ADTVariantID adt_id (unlocate variant_name)
            in Type.ADT.Variant'Named variant_name variant_id
            -- TODO: check no duplicate field names
                <$> mapM
                    (\ (field_name, ty_ast) ->
                        lift (convert_type ty_ast) >>= \ ty ->
                        pure (ID.ADTFieldID variant_id (unlocate field_name), unlocate field_name, (ty, ())))
                    fields

convert_type :: AST.Type -> MakeIRState TypeExpr
convert_type (AST.Type'Refer id) = pure $ SIR.TypeExpr'Refer () (just_span id) id
convert_type (AST.Type'Get sp prev name) = convert_type prev >>= \ prev -> pure (SIR.TypeExpr'Get () sp prev name)
convert_type (AST.Type'Tuple sp items) = mapM convert_type items >>= group_items
    where
        -- TODO: better spans for this
        group_items [a, b] = pure $ SIR.TypeExpr'Tuple () sp a b
        group_items (a:b:more) = SIR.TypeExpr'Tuple () sp a <$> group_items (b:more)
        group_items [_] = tell_error (Tuple1 sp) >> pure (SIR.TypeExpr'Poison () sp)
        group_items [] = tell_error (Tuple0 sp) >> pure (SIR.TypeExpr'Poison () sp)
convert_type (AST.Type'Hole sp id) = pure $ SIR.TypeExpr'Hole () () sp id
convert_type (AST.Type'Function sp arg res) = SIR.TypeExpr'Function () sp <$> convert_type arg <*> convert_type res
convert_type (AST.Type'Forall sp tys ty) =
    mapM new_type_var tys >>= \case
        [] -> convert_type ty -- can happen if the user passed none
        tyv1:tyv_more -> SIR.TypeExpr'Forall () sp (tyv1 :| tyv_more) <$> convert_type ty

convert_type (AST.Type'Apply sp ty args) =
    convert_type ty >>= \ ty ->
    foldlM (\ ty arg -> SIR.TypeExpr'Apply () sp ty <$> convert_type arg) ty args -- TODO: fix spans
convert_type (AST.Type'Wild sp) = pure $ SIR.TypeExpr'Wild () sp

convert_expr :: ID.ExprID -> AST.Expr -> MakeIRState Expr
convert_expr cur_id (AST.Expr'Identifier sp iden) = SIR.Expr'Identifier cur_id () sp <$> convert_path_or_single_iden iden <*> pure ()
convert_expr cur_id (AST.Expr'Char sp c) = pure (SIR.Expr'Char cur_id () sp c)
convert_expr cur_id (AST.Expr'String sp s) = pure (SIR.Expr'String cur_id () sp s)
convert_expr cur_id (AST.Expr'Int sp i) = pure (SIR.Expr'Int cur_id () sp i)
convert_expr cur_id (AST.Expr'Float sp f) = pure (SIR.Expr'Float cur_id () sp f)
convert_expr cur_id (AST.Expr'Bool sp b) = pure (SIR.Expr'Bool cur_id () sp b)

convert_expr cur_id (AST.Expr'Tuple sp items) = group_items cur_id items
    where
        group_items cur_id [a, b] = convert_expr (ID.ExprID'TupleFirstOf cur_id) a >>= \ a -> convert_expr (ID.ExprID'TupleSecondOf cur_id) b >>= \ b -> pure (SIR.Expr'Tuple cur_id () sp a b)
        group_items cur_id (a:b:more) = convert_expr (ID.ExprID'TupleFirstOf cur_id) a >>= \ a -> SIR.Expr'Tuple cur_id () sp a <$> group_items (ID.ExprID'TupleSecondOf cur_id) (b:more) -- TODO: properly do span of b:more because this just takes the span of the whole thing
        group_items cur_id [_] = tell_error (Tuple1 sp) >> pure (SIR.Expr'Poison cur_id () sp)
        group_items cur_id [] = tell_error (Tuple0 sp) >> pure (SIR.Expr'Poison cur_id () sp)

convert_expr cur_id (AST.Expr'Lambda sp params body) = convert_lambda cur_id params body
    where
        convert_lambda cur_id (param:more) body =
            convert_pattern (ID.VarParent'LambdaParam cur_id) param >>= \ param ->
            SIR.Expr'Lambda cur_id () sp param <$> convert_lambda (ID.ExprID'LambdaBodyOf cur_id) more body -- TODO: properly do spans of parts because this also just takes the whole span

        convert_lambda cur_id [] body = convert_expr cur_id body

convert_expr cur_id (AST.Expr'Let sp decls subexpr) = go cur_id decls
    where
        go cur_id [] = convert_expr cur_id subexpr
        go cur_id (first:more) =
            convert_decls (ID.VarParent'Let cur_id) (ID.DeclParent'Let cur_id) [first] >>= \ (bindings, _, _) -> -- TODO: put adts and type synonyms
            SIR.Expr'Let cur_id () sp bindings <$> go (ID.ExprID'LetResultOf cur_id) more
convert_expr cur_id (AST.Expr'LetRec sp decls subexpr) =
    convert_decls (ID.VarParent'Let cur_id) (ID.DeclParent'Let cur_id) decls >>= \ (bindings, _, _) -> -- TODO: put adts and type synonyms
    SIR.Expr'LetRec cur_id () sp bindings <$> convert_expr (ID.ExprID'LetResultOf cur_id) subexpr

convert_expr cur_id (AST.Expr'BinaryOps sp first ops) =
    SIR.Expr'BinaryOps cur_id () () sp
        <$> convert_expr (ID.ExprID'BinaryOperand cur_id 0) first
        <*> zipWithM
            (\ ind (op, right) ->
                convert_expr (ID.ExprID'BinaryOperand cur_id ind) right >>= \ right' ->
                convert_path_or_single_iden (unlocate op) >>= \ op_split_iden ->
                pure (just_span op, op_split_iden, (), right'))
            [1..]
            ops

convert_expr cur_id (AST.Expr'Call sp callee args) =
    convert_expr (ID.ExprID'CallCalleeIn cur_id) callee >>= \ callee ->
    snd <$> foldlM
        (\ (cur_id, callee) arg ->
            convert_expr (ID.ExprID'CallArgOf cur_id) arg >>= \ arg ->
            pure (ID.ExprID'CallEnclosing cur_id, SIR.Expr'Call cur_id () sp callee arg))
        (ID.ExprID'CallEnclosing cur_id, callee)
        args -- TODO: fix span for this

convert_expr cur_id (AST.Expr'If sp if_sp cond t f) = SIR.Expr'If cur_id () sp if_sp <$> convert_expr (ID.ExprID'IfCond cur_id) cond <*> convert_expr (ID.ExprID'IfTrue cur_id) t <*> convert_expr (ID.ExprID'IfFalse cur_id) f
convert_expr cur_id (AST.Expr'Match sp match_tok_sp e arms) =
    convert_expr (ID.ExprID'MatchScrutinee cur_id) e >>= \ e ->
    zipWithM
        (\ ind (pat, choice) ->
            convert_pattern (ID.VarParent'MatchArm cur_id ind) pat >>= \ pat ->
            convert_expr (ID.ExprID'MatchArm cur_id ind) choice >>= \ choice ->
            pure (pat, choice))
        [0..]
        arms
        >>= \ arms ->
    pure (SIR.Expr'Match cur_id () sp match_tok_sp e arms)

convert_expr cur_id (AST.Expr'TypeAnnotation sp ty e) = SIR.Expr'TypeAnnotation cur_id () sp <$> ((,()) <$> convert_type ty) <*> convert_expr (ID.ExprID'TypeAnnotationSubject cur_id) e
convert_expr cur_id (AST.Expr'Forall sp tys e) =
    mapM new_type_var tys >>= \case
        [] -> convert_expr (ID.ExprID'ForallResult cur_id) e
        tyv1:tyv_more -> SIR.Expr'Forall cur_id () sp (tyv1 :| tyv_more) <$> convert_expr (ID.ExprID'ForallResult cur_id) e

convert_expr cur_id (AST.Expr'TypeApply sp e args) =
    convert_expr (ID.ExprID'TypeApplyFirst cur_id) e >>= \ e ->
    snd <$> foldlM
        (\ (apply_id, e) arg ->
            convert_type arg >>= \ arg ->
            pure (ID.ExprID'TypeApplyOn apply_id, SIR.Expr'TypeApply apply_id () sp e (arg, ())))
        (ID.ExprID'TypeApplyOn cur_id, e)
        args -- TODO: fix span for this
convert_expr cur_id (AST.Expr'Hole sp hid) = pure (SIR.Expr'Hole cur_id () sp hid)

convert_pattern :: ID.VariableParent -> AST.Pattern -> MakeIRState Pattern
convert_pattern parent (AST.Pattern'Identifier located_name@(Located name_sp name)) =
    new_variable (SIR.Variable (ID.VariableID parent name) () located_name) >>= \ bn ->
    pure (SIR.Pattern'Identifier () name_sp bn)
convert_pattern _ (AST.Pattern'Wildcard sp) = pure (SIR.Pattern'Wildcard () sp)
convert_pattern parent (AST.Pattern'Tuple sp subpats) =
    mapM (convert_pattern parent) subpats >>= \ subpats' ->
    go subpats' >>= \ subpats_grouped ->
    pure subpats_grouped
    where
        go [a, b] = pure $ SIR.Pattern'Tuple () sp a b
        go (a:b:more) = SIR.Pattern'Tuple () sp a <$> go (b:more)
        go [_] = tell_error (Tuple1 sp) >> pure (SIR.Pattern'Poison () sp)
        go [] = tell_error (Tuple0 sp) >> pure (SIR.Pattern'Poison () sp)
convert_pattern parent (AST.Pattern'Named sp located_name@(Located name_sp name) at_sp subpat) =
    convert_pattern parent subpat >>= \ subpat' ->
    new_variable (SIR.Variable (ID.VariableID parent name) () located_name) >>= \ bn ->
    pure (SIR.Pattern'Named () sp at_sp (Located name_sp bn) subpat')
convert_pattern parent (AST.Pattern'AnonADTVariant sp variant fields) =
    mapM (convert_pattern parent) fields >>= \ fields ->
    convert_path_or_single_iden variant >>= \ variant_split_iden ->
    pure (SIR.Pattern'AnonADTVariant () sp variant_split_iden () [] fields)
convert_pattern parent (AST.Pattern'NamedADTVariant sp variant fields) =
    mapM (\ (field_name, field_pat) ->
            convert_pattern parent field_pat >>= \ field_pat ->
            pure (field_name, field_pat)
        ) fields >>= \ fields ->
    convert_path_or_single_iden variant >>= \ variant_split_iden ->
    pure (SIR.Pattern'NamedADTVariant () sp variant_split_iden () [] fields)

convert_path_or_single_iden :: AST.PathOrSingleIden -> MakeIRState (SIR.SplitIdentifier SIRStage (Located Text))
convert_path_or_single_iden (AST.PathOrSingleIden'Single i) = pure $ SIR.SplitIdentifier'Single i
convert_path_or_single_iden (AST.PathOrSingleIden'Path ty i) = convert_type ty >>= \ ty -> pure (SIR.SplitIdentifier'Get ty i)
