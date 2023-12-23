module UHF.Parts.ToSIR (convert) where

import UHF.Prelude

import Data.List (unzip5)

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

data Warning
    = ClassChildNotImplementedYet Span
    | InstanceChildNotImplementedYet Span

data DeclAt = DeclAt Span | ImplicitPrim deriving Show

instance Diagnostic.ToError Error where
    to_error (Tuple1 sp) = Diagnostic.Error (Just sp) "tuple of 1 element" [] []
    to_error (Tuple0 sp) = Diagnostic.Error (Just sp) "tuple of 0 elements" [] []

instance Diagnostic.ToWarning Warning where
    to_warning (ClassChildNotImplementedYet sp) = Diagnostic.Warning (Just sp) "class children are not implemented yet" [] []
    to_warning (InstanceChildNotImplementedYet sp) = Diagnostic.Warning (Just sp) "instance children are not implemented yet" [] []

type SIRStage = (Located Text, (), (), Located Text, (), Located Text, (), (), (), ())

-- TODO: remove these type aliases

type SIR = SIR.SIR SIRStage

type Module = SIR.Module SIRStage
type Binding = SIR.Binding SIRStage
type ADT = Type.ADT (TypeExpr, ())
type TypeSynonym = Type.TypeSynonym (TypeExpr, ())
type Class = Type.Class
type Instance = Type.Instance (TypeExpr, ()) (TypeExpr, ())
type TypeExpr = SIR.TypeExpr SIRStage
type Expr = SIR.Expr SIRStage
type Pattern = SIR.Pattern SIRStage
type Variable = SIR.Variable SIRStage

type ModuleArena = Arena.Arena Module SIR.ModuleKey
type ADTArena = Arena.Arena ADT Type.ADTKey
type TypeSynonymArena = Arena.Arena TypeSynonym Type.TypeSynonymKey
type ClassArena = Arena.Arena Class Type.ClassKey
type InstanceArena = Arena.Arena Instance Type.InstanceKey
type VariableArena = Arena.Arena Variable SIR.VariableKey
type QuantVarArena = Arena.Arena Type.QuantVar Type.QuantVarKey

type MakeIRState = StateT (ModuleArena, ADTArena, TypeSynonymArena, QuantVarArena, VariableArena, ClassArena, InstanceArena) (Compiler.WithDiagnostics Error Warning)

new_module :: Module -> MakeIRState SIR.ModuleKey
new_module m =
    state $ \ (mods, adts, type_synonyms, type_vars, variables, classes, instances) ->
        let (key, mods') = Arena.put m mods
        in (key, (mods', adts, type_synonyms, type_vars, variables, classes, instances))

new_adt :: ADT -> MakeIRState Type.ADTKey
new_adt adt =
    state $ \ (mods, adts, type_synonyms, type_vars, variables, classes, instances) ->
        let (key, adts') = Arena.put adt adts
        in (key, (mods, adts', type_synonyms, type_vars, variables, classes, instances))

new_type_synonym :: TypeSynonym -> MakeIRState Type.TypeSynonymKey
new_type_synonym ts =
    state $ \ (mods, adts, type_synonyms, type_vars, variables, classes, instances) ->
        let (key, type_synonyms') = Arena.put ts type_synonyms
        in (key, (mods, adts, type_synonyms', type_vars, variables, classes, instances))

new_class :: Class -> MakeIRState Type.ClassKey
new_class c =
    state $ \ (mods, adts, type_synonyms, type_vars, variables, classes, instances) ->
        let (key, classes') = Arena.put c classes
        in (key, (mods, adts, type_synonyms, type_vars, variables, classes', instances))

new_instance :: Instance -> MakeIRState Type.InstanceKey
new_instance i =
    state $ \ (mods, adts, type_synonyms, type_vars, variables, classes, instances) ->
        let (key, instances') = Arena.put i instances
        in (key, (mods, adts, type_synonyms, type_vars, variables, classes, instances'))

new_type_var :: Located Text -> MakeIRState Type.QuantVarKey
new_type_var name =
    state $ \ (mods, adts, type_synonyms, type_vars, variables, classes, instances) ->
        let (key, type_vars') = Arena.put (Type.QuantVar name) type_vars
        in (key, (mods, adts, type_synonyms, type_vars', variables, classes, instances))

new_variable :: Variable -> MakeIRState SIR.VariableKey
new_variable var =
    state $ \ (mods, adts, type_synonyms, type_vars, variables, classes, instances) ->
        let (key, variables') = Arena.put var variables
        in (key, (mods, adts, type_synonyms, type_vars, variables', classes, instances))

tell_error :: Error -> MakeIRState Compiler.ErrorReportedPromise
tell_error = lift . Compiler.tell_error

convert :: [AST.Decl] -> Compiler.WithDiagnostics Error Warning SIR
convert decls =
    runStateT
        (
            let module_id = ID.ModuleID'Root
            in convert_decls (ID.VarParent'Module module_id) (ID.DeclParent'Module module_id) decls >>= \ (bindings, adts, type_synonyms, typeclasses, instances) ->
            new_module (SIR.Module module_id bindings adts type_synonyms typeclasses instances)
        )
        (Arena.new, Arena.new, Arena.new, Arena.new, Arena.new, Arena.new, Arena.new) >>= \ (mod, (mods, adts, type_synonyms, type_vars, variables, classes, instances)) ->
    pure (SIR.SIR mods adts type_synonyms type_vars variables classes instances mod)

convert_decls :: ID.VariableParent -> ID.DeclParent -> [AST.Decl] -> MakeIRState ([Binding], [Type.ADTKey], [Type.TypeSynonymKey], [Type.ClassKey], [Type.InstanceKey])
convert_decls var_parent decl_parent decls =
    unzip5 <$> zipWithM convert_decl [0..] decls >>= \ (bindings, adts, type_synonyms, typeclasses, instances) ->
    pure (concat bindings, concat adts, concat type_synonyms, concat typeclasses, concat instances)
    where
        convert_decl :: Int -> AST.Decl -> MakeIRState ([Binding], [Type.ADTKey], [Type.TypeSynonymKey], [Type.ClassKey], [Type.InstanceKey])
        convert_decl ind (AST.Decl'Value target eq_sp expr) =
            convert_expr (ID.ExprID'InitializerOf decl_parent ind) expr >>= \ expr' ->
            convert_pattern var_parent target >>= \ target' ->
            pure ([SIR.Binding target' eq_sp expr'], [], [], [], [])

        convert_decl _ (AST.Decl'Data l_data_name@(Located _ data_name) type_params variants) =
            mapM new_type_var type_params >>= \ ty_param_vars ->

            let adt_id = ID.DeclID decl_parent data_name
            in mapM (convert_variant adt_id) variants >>= \ variants_converted ->
            let adt = Type.ADT adt_id l_data_name ty_param_vars variants_converted
            in

            new_adt adt >>= \ adt_key ->

            pure ([], [adt_key], [], [], [])

        convert_decl _ (AST.Decl'TypeSyn l_name@(Located _ name) expansion) =
            convert_type expansion >>= \ expansion' ->
            new_type_synonym (Type.TypeSynonym (ID.DeclID decl_parent name) l_name (expansion', ())) >>= \ tsyn_key ->
            pure ([], [], [tsyn_key], [], [])

        convert_decl _ (AST.Decl'Class l_class_name@(Located class_name_sp class_name) type_params subdecls) = do
            let id = ID.DeclID decl_parent class_name
            ty_param_quant_vars <- mapM new_type_var type_params
            mapM_ (const (lift $ Compiler.tell_warning (ClassChildNotImplementedYet class_name_sp))) subdecls -- TODO: do this span properly, TODO: implement this and then remove the warning
            class_key <- new_class $ Type.Class id l_class_name ty_param_quant_vars (map (const ()) subdecls) -- TODO: figure out how subdecls are supposed to work
            pure ([], [], [], [class_key], [])

        convert_decl _ (AST.Decl'Instance type_params class_ args subdecls) = do
            ty_param_quant_vars <- mapM new_type_var type_params
            class_converted <- convert_type class_
            args <- mapM convert_type args
            mapM_ (const (lift $ Compiler.tell_warning (InstanceChildNotImplementedYet (AST.type_span class_)))) subdecls -- TODO: do this span properly, TODO: implement this and then remove the warning
            instance_key <- new_instance $ Type.Instance ty_param_quant_vars (class_converted, ()) (map (,()) args) (map (const ()) subdecls) -- TODO: figure out how subdecls are supposed to work
            pure ([], [], [], [], [instance_key])

        convert_variant adt_id (AST.DataVariant'Anon variant_name fields) =
            let variant_id = ID.ADTVariantID adt_id (unlocate variant_name)
            in Type.ADT.Variant'Anon variant_name variant_id
                <$> zipWithM
                    (\ field_idx ty_ast ->
                        convert_type ty_ast >>= \ ty ->
                        pure (ID.ADTFieldID variant_id (show (field_idx :: Int)), (ty, ())))
                    [0..]
                    fields
        convert_variant adt_id (AST.DataVariant'Named variant_name fields) =
            let variant_id = ID.ADTVariantID adt_id (unlocate variant_name)
            in Type.ADT.Variant'Named variant_name variant_id
            -- TODO: check no duplicate field names
                <$> mapM
                    (\ (field_name, ty_ast) ->
                        convert_type ty_ast >>= \ ty ->
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
            convert_decls (ID.VarParent'Let cur_id) (ID.DeclParent'Let cur_id) [first] >>= \ (bindings, adts, type_synonyms, typeclasses, instances) ->
            SIR.Expr'Let cur_id () sp bindings adts type_synonyms typeclasses instances <$> go (ID.ExprID'LetResultOf cur_id) more
convert_expr cur_id (AST.Expr'LetRec sp decls subexpr) =
    convert_decls (ID.VarParent'Let cur_id) (ID.DeclParent'Let cur_id) decls >>= \ (bindings, adts, type_synonyms, typeclasses, instances) ->
    SIR.Expr'LetRec cur_id () sp bindings adts type_synonyms typeclasses instances <$> convert_expr (ID.ExprID'LetResultOf cur_id) subexpr

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
