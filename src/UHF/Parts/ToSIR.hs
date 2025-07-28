module UHF.Parts.ToSIR (convert) where

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
import qualified UHF.Data.Token as Token
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Util.Arena as Arena

data Error
    = Tuple1 Span
    | Tuple0 Span
    | NoMain -- TODO: add a span to this? span of the whole module?
    | MultipleMains [Span]

data DeclAt = DeclAt Span | ImplicitPrim deriving Show

instance Diagnostic.ToError Error where
    to_error (Tuple1 sp) = Diagnostic.Error (Just sp) "tuple of 1 element" [] []
    to_error (Tuple0 sp) = Diagnostic.Error (Just sp) "tuple of 0 elements" [] []
    to_error NoMain = Diagnostic.Error Nothing "no main function" [] []
    to_error (MultipleMains sps) = Diagnostic.Error (Just $ head sps) "multiple main functions" (map (\ sp -> (Just sp, Diagnostic.MsgError, Nothing)) sps) []

type SIRStage = ((), (), (), (), (), (), (), (), (), (), ())

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

tell_error :: Error -> MakeIRState Compiler.ErrorReportedPromise
tell_error = lift . Compiler.tell_error

convert :: [AST.Decl] -> Compiler.WithDiagnostics Error Void SIR
convert decls = do
    (root_module, (mods, adts, type_synonyms, type_vars, variables)) <-
        runStateT
            (
                let module_id = ID.ModuleID'Root
                in convert_decls (ID.VarParent'Module module_id) (ID.DeclParent'Module module_id) decls >>= \ (bindings, adts, type_synonyms) ->
                new_module (SIR.Module module_id bindings adts type_synonyms)
            )
            (Arena.new, Arena.new, Arena.new, Arena.new, Arena.new)
    main_function <- search_for_main_function mods variables root_module
    pure (SIR.SIR mods adts type_synonyms type_vars variables (SIR.CU root_module main_function))

search_for_main_function :: ModuleArena -> VariableArena -> SIR.ModuleKey -> Compiler.WithDiagnostics Error Void (Maybe SIR.VariableKey)
search_for_main_function mods variables mod =
    let (SIR.Module _ bindings _ _) = Arena.get mods mod
        variables_called_main = bindings &
            concatMap (\ (SIR.Binding pat _ _) -> go_pat pat)
    in case variables_called_main of
        [] -> do
            _ <- Compiler.tell_error NoMain
            pure Nothing
        [main] -> pure $ Just main
        multiple -> do
            _ <- Compiler.tell_error $ MultipleMains $ map get_var_span multiple
            pure Nothing
    where
        go_pat :: SIR.Pattern stage -> [SIR.VariableKey]
        go_pat (SIR.Pattern'Variable _ _ vk) = go_var vk
        go_pat (SIR.Pattern'Wildcard _ _) = []
        go_pat (SIR.Pattern'Tuple _ _ a b) = go_pat a ++ go_pat b
        go_pat (SIR.Pattern'Named _ _ _ (Located _ vk) subpat) = go_var vk ++ go_pat subpat
        go_pat (SIR.Pattern'AnonADTVariant _ _ _ _ _ field_pats) = concatMap go_pat field_pats
        go_pat (SIR.Pattern'NamedADTVariant _ _ _ _ _ field_pats) = concatMap (go_pat . snd) field_pats
        go_pat (SIR.Pattern'Poison _ _) = []

        go_var vk =
            let (SIR.Variable _ _ (Located _ name)) = Arena.get variables vk
            in if name == "main" then [vk] else []

        get_var_span vk =
            let (SIR.Variable _ _ (Located sp _)) = Arena.get variables vk
            in sp

convert_decls :: ID.VariableParent -> ID.DeclParent -> [AST.Decl] -> MakeIRState ([Binding], [Type.ADTKey], [Type.TypeSynonymKey])
convert_decls var_parent decl_parent decls =
    unzip3 <$> zipWithM convert_decl [0..] decls >>= \ (bindings, adts, type_synonyms) ->
    pure (concat bindings, concat adts, concat type_synonyms)
    where
        convert_decl :: Int -> AST.Decl -> MakeIRState ([Binding], [Type.ADTKey], [Type.TypeSynonymKey])
        convert_decl ind (AST.Decl'Value _ target (Located eq_sp _) expr) =
            convert_expr (ID.ExprID'InitializerOf decl_parent ind) expr >>= \ expr' ->
            convert_pattern var_parent target >>= \ target' ->
            pure ([SIR.Binding target' eq_sp expr'], [], [])

        convert_decl _ (AST.Decl'Data _ l_data_name@(Located _ data_name) type_params variants) =
            runMaybeT (
                mapM (lift . new_type_var . fmap convert_aiden_tok) type_params >>= \ ty_param_vars ->

                let adt_id = ID.DeclID decl_parent (convert_aiden_tok data_name)
                in mapM (convert_variant adt_id) variants >>= \ variants_converted ->
                let adt = Type.ADT adt_id (convert_aiden_tok <$> l_data_name) ty_param_vars variants_converted
                in

                lift (new_adt adt) >>= \ adt_key ->

                pure adt_key
            ) >>= \case
                Just adt_key -> pure ([], [adt_key], [])
                Nothing -> pure ([], [], [])

        convert_decl _ (AST.Decl'TypeSyn _ l_name@(Located _ name) expansion) =
            runMaybeT (
                lift (convert_type expansion) >>= \ expansion' ->
                lift (new_type_synonym (Type.TypeSynonym (ID.DeclID decl_parent (convert_aiden_tok name)) (convert_aiden_tok <$> l_name) (expansion', ())))
            ) >>= \case
                Just syn_key -> pure ([], [], [syn_key])
                Nothing -> pure ([], [], [])

        convert_variant adt_id (AST.DataVariant'Anon (Located variant_name_sp (Token.AlphaIdentifier variant_name)) fields) =
            let variant_id = ID.ADTVariantID adt_id variant_name
            in Type.ADT.Variant'Anon (Located variant_name_sp variant_name) variant_id
                <$> zipWithM
                    (\ field_idx ty_ast ->
                        lift (convert_type ty_ast) >>= \ ty ->
                        pure (ID.ADTFieldID variant_id (show (field_idx :: Int)), (ty, ())))
                    [0..]
                    fields
        convert_variant adt_id (AST.DataVariant'Named (Located variant_name_sp (Token.AlphaIdentifier variant_name)) fields) =
            let variant_id = ID.ADTVariantID adt_id variant_name
            in Type.ADT.Variant'Named (Located variant_name_sp variant_name) variant_id
            -- TODO: check no duplicate field names
                <$> mapM
                    (\ (Located _ (Token.AlphaIdentifier field_name), ty_ast) ->
                        lift (convert_type ty_ast) >>= \ ty ->
                        pure (ID.ADTFieldID variant_id field_name, field_name, (ty, ())))
                    fields

convert_type :: AST.Type -> MakeIRState TypeExpr
convert_type (AST.Type'Refer id) = pure $ SIR.TypeExpr'Refer () (just_span id) () (convert_aiden_tok <$> id) ()
convert_type (AST.Type'Get sp prev name) = convert_type prev >>= \ prev -> pure (SIR.TypeExpr'Get () sp prev (convert_aiden_tok <$> name))
convert_type (AST.Type'Tuple sp items) = mapM convert_type items >>= group_items
    where
        -- TODO: better spans for this
        group_items [a, b] = pure $ SIR.TypeExpr'Tuple () sp a b
        group_items (a:b:more) = SIR.TypeExpr'Tuple () sp a <$> group_items (b:more)
        group_items [_] = tell_error (Tuple1 sp) >> pure (SIR.TypeExpr'Poison () sp)
        group_items [] = tell_error (Tuple0 sp) >> pure (SIR.TypeExpr'Poison () sp)
convert_type (AST.Type'Hole sp id) = pure $ SIR.TypeExpr'Hole () () sp (convert_aiden_tok <$> id)
convert_type (AST.Type'Function sp arg res) = SIR.TypeExpr'Function () sp <$> convert_type arg <*> convert_type res
convert_type (AST.Type'Forall sp tys ty) =
    mapM (new_type_var . fmap convert_aiden_tok) tys >>= \case
        [] -> convert_type ty -- can happen if the user passed none
        tyv1:tyv_more -> SIR.TypeExpr'Forall () sp (tyv1 :| tyv_more) <$> convert_type ty

convert_type (AST.Type'Apply sp ty args) =
    convert_type ty >>= \ ty ->
    foldlM (\ ty arg -> SIR.TypeExpr'Apply () sp ty <$> convert_type arg) ty args -- TODO: fix spans
convert_type (AST.Type'Wild sp) = pure $ SIR.TypeExpr'Wild () sp

convert_expr :: ID.ExprID -> AST.Expr -> MakeIRState Expr
convert_expr cur_id (AST.Expr'ReferAlpha sp t i) = SIR.Expr'Refer cur_id () sp <$> make_split_identifier t (convert_aiden_tok <$> i) <*> pure ()
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
            convert_decls (ID.VarParent'Let cur_id) (ID.DeclParent'Let cur_id) [first] >>= \ (bindings, adts, type_synonyms) ->
            SIR.Expr'Let cur_id () sp bindings adts type_synonyms <$> go (ID.ExprID'LetResultOf cur_id) more
convert_expr cur_id (AST.Expr'LetRec sp decls subexpr) =
    convert_decls (ID.VarParent'Let cur_id) (ID.DeclParent'Let cur_id) decls >>= \ (bindings, adts, type_synonyms) ->
    SIR.Expr'LetRec cur_id () sp bindings adts type_synonyms <$> convert_expr (ID.ExprID'LetResultOf cur_id) subexpr
convert_expr cur_id (AST.Expr'Where sp subexpr decls) =
    convert_decls (ID.VarParent'Where cur_id) (ID.DeclParent'Where cur_id) decls >>= \ (bindings, adts, type_synonyms) ->
    SIR.Expr'LetRec cur_id () sp bindings adts type_synonyms <$> convert_expr (ID.ExprID'WhereResultOf cur_id) subexpr

convert_expr cur_id (AST.Expr'BinaryOps sp first ops) =
    SIR.Expr'BinaryOps cur_id () () sp
        <$> convert_expr (ID.ExprID'BinaryOperand cur_id 0) first
        <*> zipWithM
            (\ ind (op, right) -> do
                right' <- convert_expr (ID.ExprID'BinaryOperand cur_id ind) right
                case op of
                    AST.Operator'Path op_sp op_ty op_last -> do
                        op_ty <- convert_type op_ty
                        pure (op_sp, SIR.SplitIdentifier'Get op_ty (convert_siden_tok <$> op_last), (), right')
                    AST.Operator'Single op_iden@(Located op_sp _) -> pure (op_sp, SIR.SplitIdentifier'Single () (convert_siden_tok <$> op_iden) (), (), right'))
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

convert_expr cur_id (AST.Expr'If sp (Located if_sp _) cond t f) = SIR.Expr'If cur_id () sp if_sp <$> convert_expr (ID.ExprID'IfCond cur_id) cond <*> convert_expr (ID.ExprID'IfTrue cur_id) t <*> convert_expr (ID.ExprID'IfFalse cur_id) f
convert_expr cur_id (AST.Expr'Match sp (Located match_tok_sp _) e arms) =
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
    mapM (new_type_var . fmap convert_aiden_tok) tys >>= \case
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
convert_expr cur_id (AST.Expr'Hole sp hid) = pure (SIR.Expr'Hole cur_id () sp (convert_aiden_tok <$> hid))

convert_pattern :: ID.VariableParent -> AST.Pattern -> MakeIRState Pattern
convert_pattern parent (AST.Pattern'AlphaVar located_name@(Located name_sp (Token.AlphaIdentifier name))) =
    new_variable (SIR.Variable (ID.VariableID parent name) () (convert_aiden_tok <$> located_name)) >>= \ bn ->
    pure (SIR.Pattern'Variable () name_sp bn)
convert_pattern _ (AST.Pattern'Wildcard (Located underscore_sp _)) = pure (SIR.Pattern'Wildcard () underscore_sp)
convert_pattern parent (AST.Pattern'Tuple sp subpats) =
    mapM (convert_pattern parent) subpats >>= \ subpats' ->
    go subpats' >>= \ subpats_grouped ->
    pure subpats_grouped
    where
        go [a, b] = pure $ SIR.Pattern'Tuple () sp a b
        go (a:b:more) = SIR.Pattern'Tuple () sp a <$> go (b:more)
        go [_] = tell_error (Tuple1 sp) >> pure (SIR.Pattern'Poison () sp)
        go [] = tell_error (Tuple0 sp) >> pure (SIR.Pattern'Poison () sp)
convert_pattern parent (AST.Pattern'NamedAlpha sp located_name@(Located name_sp name) (Located at_sp _) subpat) =
    convert_pattern parent subpat >>= \ subpat' ->
    new_variable (SIR.Variable (ID.VariableID parent (convert_aiden_tok name)) () (convert_aiden_tok <$> located_name)) >>= \ bn ->
    pure (SIR.Pattern'Named () sp at_sp (Located name_sp bn) subpat')
convert_pattern parent (AST.Pattern'AnonADTVariant sp v_ty variant fields) = do
    fields <- mapM (convert_pattern parent) fields
    variant_split_iden <- make_split_identifier v_ty (convert_aiden_tok <$> variant)
    pure (SIR.Pattern'AnonADTVariant () sp variant_split_iden () [] fields)
convert_pattern parent (AST.Pattern'NamedADTVariant sp v_ty variant fields) = do
    fields <- mapM (\ (field_name, field_pat) ->
            convert_pattern parent field_pat >>= \ field_pat ->
            pure (convert_aiden_tok <$> field_name, field_pat)
        ) fields
    variant_split_iden <- make_split_identifier v_ty (convert_aiden_tok <$> variant)
    pure (SIR.Pattern'NamedADTVariant () sp variant_split_iden () [] fields)

make_split_identifier :: Maybe AST.Type -> Located Text -> MakeIRState (SIR.SplitIdentifier () SIRStage)
make_split_identifier Nothing i = pure $ SIR.SplitIdentifier'Single () i ()
make_split_identifier (Just ty) i = do
    ty <- convert_type ty
    pure $ SIR.SplitIdentifier'Get ty i

convert_aiden_tok :: Token.AlphaIdentifier -> Text
convert_aiden_tok (Token.AlphaIdentifier i) = i
convert_siden_tok :: Token.SymbolIdentifier -> Text
convert_siden_tok (Token.SymbolIdentifier i) = i
convert_kiden_tok :: Token.KeywordIdentifier -> Text
convert_kiden_tok (Token.KeywordIdentifier i) = i
