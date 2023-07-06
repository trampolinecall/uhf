module UHF.Phases.ToSIR (convert) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.Data.AST as AST
import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.ID as ID

import UHF.IO.Span (Span)
import UHF.IO.Located (Located (..))

import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Codes

import qualified UHF.Compiler as Compiler

import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)

data Error
    = PathInPattern (Located [Located Text]) -- TODO: make this less repetitive
    | PathInTypeName (Located [Located Text])
    | PathInVariantName (Located [Located Text])
    | PathInFieldName (Located [Located Text])

    | Tuple1 Span
    | Tuple0 Span

data DeclAt = DeclAt Span | ImplicitPrim deriving Show

instance Diagnostic.ToError Error where
    to_error (PathInPattern (Located sp _)) = Diagnostic.Error Codes.binding_lhs_path (Just sp) "path in pattern" [] []
    to_error (PathInTypeName (Located sp _)) = Diagnostic.Error Codes.path_in_type_name (Just sp) "path in type name" [] []
    to_error (PathInVariantName (Located sp _)) = Diagnostic.Error Codes.path_in_variant_name (Just sp) "path in 'data' variant name" [] []
    to_error (PathInFieldName (Located sp _)) = Diagnostic.Error Codes.path_in_field_name (Just sp) "path in field name" [] []

    to_error (Tuple1 sp) = Diagnostic.Error Codes.tuple1 (Just sp) "tuple of 1 element" [] []
    to_error (Tuple0 sp) = Diagnostic.Error Codes.tuple0 (Just sp) "tuple of 0 elements" [] []

type SIR = SIR.SIR Identifier Identifier Identifier () ()

type Identifier = [Located Text]
type Decl = SIR.Decl
type Module = SIR.Module Identifier Identifier Identifier () ()
type Binding = SIR.Binding Identifier Identifier Identifier () ()
type ADT = Type.ADT TypeExpr
type TypeSynonym = Type.TypeSynonym TypeExpr
type TypeExpr = SIR.TypeExpr Identifier ()
type Expr = SIR.Expr Identifier Identifier Identifier () ()
type Pattern = SIR.Pattern Identifier ()
type BoundValue = SIR.BoundValue ()

type DeclArena = Arena.Arena Decl SIR.DeclKey
type ModuleArena = Arena.Arena Module SIR.ModuleKey
type ADTArena = Arena.Arena ADT Type.ADTKey
type TypeSynonymArena = Arena.Arena TypeSynonym Type.TypeSynonymKey
type BoundValueArena = Arena.Arena BoundValue SIR.BoundValueKey
type TypeVarArena = Arena.Arena Type.Var Type.TypeVarKey

type MakeIRState = StateT (DeclArena, ModuleArena, ADTArena, TypeSynonymArena, TypeVarArena, BoundValueArena) (Compiler.WithDiagnostics Error Void)

new_module :: Module -> MakeIRState SIR.ModuleKey
new_module m =
    state $ \ (decls, mods, adts, type_synonyms, type_vars, bound_values) ->
        let (key, mods') = Arena.put m mods
        in (key, (decls, mods', adts, type_synonyms, type_vars, bound_values))

new_adt :: ADT -> MakeIRState Type.ADTKey
new_adt adt =
    state $ \ (decls, mods, adts, type_synonyms, type_vars, bound_values) ->
        let (key, adts') = Arena.put adt adts
        in (key, (decls, mods, adts', type_synonyms, type_vars, bound_values))

new_type_synonym :: TypeSynonym -> MakeIRState Type.TypeSynonymKey
new_type_synonym ts =
    state $ \ (decls, mods, adts, type_synonyms, type_vars, bound_values) ->
        let (key, type_synonyms') = Arena.put ts type_synonyms
        in (key, (decls, mods, adts, type_synonyms', type_vars, bound_values))

new_type_var :: Located Text -> MakeIRState Type.TypeVarKey
new_type_var name =
    state $ \ (decls, mods, adts, type_synonyms, type_vars, bound_values) ->
        let (key, type_vars') = Arena.put (Type.Var name) type_vars
        in (key, (decls, mods, adts, type_synonyms, type_vars', bound_values))

new_bound_value :: BoundValue -> MakeIRState SIR.BoundValueKey
new_bound_value bv =
    state $ \ (decls, mods, adts, type_synonyms, type_vars, bound_values) ->
        let (key, bound_values') = Arena.put bv bound_values
        in (key, (decls, mods, adts, type_synonyms, type_vars, bound_values'))

tell_error :: Error -> MakeIRState ()
tell_error = lift . Compiler.tell_error

make_iden1 :: Located [Located Text] -> Maybe (Located Text)
make_iden1 (Located _ [iden1]) = Just iden1
make_iden1 _ = Nothing

make_iden1_with_err :: (Located [Located Text] -> Error) -> Located [Located Text] -> MakeIRState (Maybe (Located Text))
make_iden1_with_err make_err iden =
    case make_iden1 iden of
        Just res -> pure $ Just res
        Nothing -> tell_error (make_err iden) >> pure Nothing

convert :: [AST.Decl] -> Compiler.WithDiagnostics Error Void SIR
convert decls =
    runStateT
        (
            let module_id = ID.ModuleID [] -- TODO: figure out how the module system is going to work
            in convert_decls (ID.BVParent'Module module_id) (ID.DeclParent'Module module_id) decls >>= \ (bindings, adts, type_synonyms) ->
            new_module (SIR.Module module_id bindings adts type_synonyms)
        )
        (Arena.new, Arena.new, Arena.new, Arena.new, Arena.new, Arena.new) >>= \ (mod, (decls, mods, adts, type_synonyms, type_vars, bound_values)) ->
    pure (SIR.SIR decls mods adts type_synonyms type_vars bound_values mod)

convert_decls :: ID.BoundValueParent -> ID.DeclParent -> [AST.Decl] -> MakeIRState ([Binding], [Type.ADTKey], [Type.TypeSynonymKey])
convert_decls bv_parent decl_parent decls =
    unzip3 <$> zipWithM convert_decl [0..] decls >>= \ (bindings, adts, type_synonyms) ->
    pure (concat bindings, concat adts, concat type_synonyms)
    where
        convert_decl :: Int -> AST.Decl -> MakeIRState ([Binding], [Type.ADTKey], [Type.TypeSynonymKey])
        convert_decl ind (AST.Decl'Value target eq_sp expr) =
            convert_expr (ID.ExprID'InitializerOf decl_parent ind) expr >>= \ expr' ->
            convert_pattern bv_parent target >>= \ (target') ->
            pure ([SIR.Binding target' eq_sp expr'], [], [])

        convert_decl _ (AST.Decl'Data name type_params variants) =
            runMaybeT (
                mapM (iden1_for_type_name) type_params >>= \ ty_param_names ->
                mapM (lift . new_type_var) ty_param_names >>= \ ty_param_vars ->

                iden1_for_type_name name >>= \ l_data_name@(Located _ data_name) ->
                mapM convert_variant variants >>= \ variants_converted ->
                let datatype = Type.ADT (ID.DeclID decl_parent data_name) l_data_name ty_param_vars variants_converted
                in

                lift (new_adt datatype) >>= \ adt_key ->

                (catMaybes <$> mapM
                    (\ case
                        (Type.ADTVariant'Anon (Located name_sp name) _, index) ->
                            let variant_index = Type.ADTVariantIndex adt_key index
                            in lift (new_bound_value (SIR.BoundValue'ADTVariant (ID.BoundValueID bv_parent name) variant_index () name_sp)) >>= \ bv_key ->
                            pure (Just (SIR.Binding'ADTVariant name_sp bv_key variant_index))
                        (Type.ADTVariant'Named _ _, _) -> pure Nothing
                    )
                    (zip variants_converted [0..])) >>= \ constructor_bindings ->

                pure (adt_key, constructor_bindings)
            ) >>= \case
                Just (adt_key, constructor_bindings) -> pure (constructor_bindings, [adt_key], [])
                Nothing -> pure ([], [], [])

        convert_decl _ (AST.Decl'TypeSyn name expansion) =
            runMaybeT (
                lift (convert_type expansion) >>= \ expansion' ->
                iden1_for_type_name name >>= \ l_syn_name@(Located _ syn_name) ->
                lift (new_type_synonym (Type.TypeSynonym (ID.DeclID decl_parent syn_name) l_syn_name expansion'))
            ) >>= \case
                Just syn_key -> pure ([], [], [syn_key])
                Nothing -> pure ([], [], [])

        iden1_for_variant_name = MaybeT . make_iden1_with_err PathInVariantName
        iden1_for_type_name = MaybeT . make_iden1_with_err PathInTypeName
        iden1_for_field_name = MaybeT . make_iden1_with_err PathInFieldName

        convert_variant (AST.DataVariant'Anon name fields) = Type.ADTVariant'Anon <$> (iden1_for_variant_name name) <*> lift (mapM convert_type fields)
        convert_variant (AST.DataVariant'Named name fields) =
            Type.ADTVariant'Named
                <$> (iden1_for_variant_name name)
                <*> mapM
                    (\ (field_name, ty_ast) ->
                        (,)
                            <$> (unlocate <$> iden1_for_field_name field_name)
                            <*> lift (convert_type ty_ast))
                    fields

convert_type :: AST.Type -> MakeIRState TypeExpr
convert_type (AST.Type'Identifier id) = pure $ SIR.TypeExpr'Identifier () (just_span id) (unlocate id)
convert_type (AST.Type'Tuple sp items) = mapM (convert_type) items >>= group_items
    where
        group_items [a, b] = pure $ SIR.TypeExpr'Tuple () a b
        group_items (a:b:more) = SIR.TypeExpr'Tuple () a <$> group_items (b:more)
        group_items [_] = tell_error (Tuple1 sp) >> pure (SIR.TypeExpr'Poison () sp)
        group_items [] = tell_error (Tuple0 sp) >> pure (SIR.TypeExpr'Poison () sp)
convert_type (AST.Type'Hole sp id) = pure $ SIR.TypeExpr'Hole () sp id
convert_type (AST.Type'Function sp arg res) = SIR.TypeExpr'Function () sp <$> convert_type arg <*> convert_type res
convert_type (AST.Type'Forall _ tys ty) =
    catMaybes <$> mapM (make_iden1_with_err PathInTypeName) tys >>= \ tys ->
    mapM new_type_var tys >>= \ ty_vars ->
    case ty_vars of
        [] -> convert_type ty -- can happen if there are errors in all the type names or if the user passed none
        tyv1:tyv_more -> SIR.TypeExpr'Forall () (tyv1 :| tyv_more) <$> convert_type ty

convert_type (AST.Type'Apply sp ty args) =
    convert_type ty >>= \ ty ->
    foldlM (\ ty arg -> SIR.TypeExpr'Apply () sp ty <$> convert_type arg) ty args -- TODO: fix spans
convert_type (AST.Type'Wild sp) = pure $ SIR.TypeExpr'Wild () sp

convert_expr :: ID.ExprID -> AST.Expr -> MakeIRState Expr
convert_expr cur_id (AST.Expr'Identifier iden) = pure (SIR.Expr'Identifier cur_id () (just_span iden) (unlocate iden))
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
            convert_pattern (ID.BVParent'LambdaParam cur_id) param >>= \ (param) ->
            SIR.Expr'Lambda cur_id () sp param <$> convert_lambda (ID.ExprID'LambdaBodyOf cur_id) more body -- TODO: properly do spans of parts because this also just takes the whole span

        convert_lambda cur_id [] body = convert_expr cur_id body

convert_expr cur_id (AST.Expr'Let sp decls subexpr) = go cur_id decls
    where
        go cur_id [] = convert_expr cur_id subexpr
        go cur_id (first:more) =
            convert_decls (ID.BVParent'Let cur_id) (ID.DeclParent'Let cur_id) [first] >>= \ (bindings, _, _) -> -- TODO: put adts and type synonyms
            SIR.Expr'Let cur_id () sp bindings <$> go (ID.ExprID'LetResultOf cur_id) more
convert_expr cur_id (AST.Expr'LetRec sp decls subexpr) =
    convert_decls (ID.BVParent'Let cur_id) (ID.DeclParent'Let cur_id) decls >>= \ (bindings, _, _) -> -- TODO: put adts and type synonyms
    SIR.Expr'LetRec cur_id () sp bindings <$> convert_expr (ID.ExprID'LetResultOf cur_id) subexpr

convert_expr cur_id (AST.Expr'BinaryOps sp first ops) = SIR.Expr'BinaryOps cur_id () () sp <$> convert_expr (ID.ExprID'BinaryOperand cur_id 0) first <*> zipWithM (\ ind (op, right) -> convert_expr (ID.ExprID'BinaryOperand cur_id ind) right >>= \ right' -> pure ((unlocate op), right')) [1..] ops

convert_expr cur_id (AST.Expr'Call sp callee args) =
    convert_expr (ID.ExprID'CallCalleeIn cur_id) callee >>= \ callee ->
    snd <$> foldlM
        (\ (cur_id, callee) arg ->
            convert_expr (ID.ExprID'CallArgOf cur_id) arg >>= \ arg ->
            pure (ID.ExprID'CallEnclosing cur_id, SIR.Expr'Call cur_id () sp callee arg))
        (ID.ExprID'CallEnclosing cur_id, callee)
        args -- TODO: fix span for this

convert_expr cur_id (AST.Expr'If sp if_sp cond t f) = SIR.Expr'If cur_id () sp if_sp <$> convert_expr (ID.ExprID'IfCond cur_id) cond <*> convert_expr (ID.ExprID'IfTrue cur_id) t <*> convert_expr (ID.ExprID'IfFalse cur_id) f
convert_expr cur_id (AST.Expr'Case sp case_sp e arms) =
    convert_expr (ID.ExprID'CaseScrutinee cur_id) e >>= \ e ->
    zipWithM
        (\ ind (pat, choice) ->
            convert_pattern (ID.BVParent'CaseArm cur_id ind) pat >>= \ pat ->
            convert_expr (ID.ExprID'CaseArm cur_id ind) choice >>= \ choice ->
            pure (pat, choice))
        [0..]
        arms
        >>= \ arms ->
    pure (SIR.Expr'Case cur_id () sp case_sp e arms)

convert_expr cur_id (AST.Expr'TypeAnnotation sp ty e) = SIR.Expr'TypeAnnotation cur_id () sp <$> convert_type ty <*> convert_expr (ID.ExprID'TypeAnnotationSubject cur_id) e
convert_expr cur_id (AST.Expr'Forall sp tys e) =
    catMaybes <$> mapM (make_iden1_with_err PathInTypeName) tys >>= \ tys ->
    mapM new_type_var tys >>= \ ty_vars ->
    case ty_vars of
        [] -> convert_expr (ID.ExprID'ForallResult cur_id) e
        tyv1:tyv_more -> SIR.Expr'Forall cur_id () sp (tyv1 :| tyv_more) <$> convert_expr (ID.ExprID'ForallResult cur_id) e

convert_expr cur_id (AST.Expr'TypeApply sp e args) =
    convert_expr (ID.ExprID'TypeApplyFirst cur_id) e >>= \ e ->
    snd <$> foldlM
        (\ (apply_id, e) arg ->
            convert_type arg >>= \ arg ->
            pure (ID.ExprID'TypeApplyOn apply_id, SIR.Expr'TypeApply apply_id () sp e arg))
        (ID.ExprID'TypeApplyOn cur_id, e)
        args -- TODO: fix span for this
convert_expr cur_id (AST.Expr'Hole sp hid) = pure (SIR.Expr'Hole cur_id () sp hid)

convert_pattern :: ID.BoundValueParent -> AST.Pattern -> MakeIRState Pattern
convert_pattern parent (AST.Pattern'Identifier iden) =
    make_iden1_with_err PathInPattern iden >>= \case
        Just (located_name@(Located name_sp name)) ->
            new_bound_value (SIR.BoundValue (ID.BoundValueID parent name) () located_name) >>= \ bn ->
            pure (SIR.Pattern'Identifier () name_sp bn)

        Nothing -> pure (SIR.Pattern'Poison () (just_span iden))
convert_pattern _ (AST.Pattern'Wildcard sp) = pure (SIR.Pattern'Wildcard () sp)
convert_pattern parent (AST.Pattern'Tuple sp subpats) =
    mapM (convert_pattern parent) subpats >>= \ subpats' ->
    go subpats' >>= \ subpats_grouped ->
    pure (subpats_grouped)
    where
        go [a, b] = pure $ SIR.Pattern'Tuple () sp a b
        go (a:b:more) = SIR.Pattern'Tuple () sp a <$> go (b:more)
        go [_] = tell_error (Tuple1 sp) >> pure (SIR.Pattern'Poison () sp)
        go [] = tell_error (Tuple0 sp) >> pure (SIR.Pattern'Poison () sp)
convert_pattern parent (AST.Pattern'Named sp iden at_sp subpat) =
    convert_pattern parent subpat >>= \ (subpat') ->
    make_iden1_with_err PathInPattern iden >>= \case
        Just (located_name@(Located name_sp name)) ->
            new_bound_value (SIR.BoundValue (ID.BoundValueID parent name) () located_name) >>= \ bn ->
            pure (SIR.Pattern'Named () sp at_sp (Located name_sp bn) subpat')

        Nothing -> pure (SIR.Pattern'Poison () sp)
convert_pattern parent (AST.Pattern'AnonADTVariant sp iden fields) =
    mapM (convert_pattern parent) fields >>= \ (fields) ->
    pure (SIR.Pattern'AnonADTVariant () sp (unlocate iden) fields)
convert_pattern parent (AST.Pattern'NamedADTVariant sp iden fields) =
    mapM (\ (field_name, field_pat) ->
        make_iden1_with_err PathInFieldName field_name >>= \case
            Just field_name ->
                convert_pattern parent field_pat >>= \ field_pat ->
                pure (Just (field_name, field_pat))
            Nothing -> pure Nothing
        ) fields >>= \ fields ->
    case sequence fields of
        Just fields -> pure (SIR.Pattern'NamedADTVariant () sp (unlocate iden) fields)
        Nothing -> pure (SIR.Pattern'Poison () sp)
