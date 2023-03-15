module UHF.Phases.Middle.ToHIR (convert) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.Data.AST as AST
import qualified UHF.Data.IR.HIR as HIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.IR.IDGen as IDGen
import UHF.Data.IR.Keys

import UHF.IO.Span (Span)
import UHF.IO.Located (Located (..))

import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Codes

import qualified Data.Map as Map
import qualified Data.List as List

import qualified UHF.Compiler as Compiler

import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Control.Monad.Fix (mfix)

data Error
    = MultipleDecls Text [DeclAt]
    | PathInPattern (Located [Located Text]) -- TODO: make this less repetitive
    | PathInTypeName (Located [Located Text])
    | PathInVariantName (Located [Located Text])
    | PathInFieldName (Located [Located Text])

    | Tuple1 Span
    | Tuple0 Span

data DeclAt = DeclAt Span | ImplicitPrim deriving Show

instance Diagnostic.ToError Error where
    to_error (MultipleDecls name decl_ats) =
        let last_decl = last decl_ats
        in Diagnostic.Error
            Codes.multiple_decls
            (decl_at_span last_decl)
            (show (length decl_ats) <> " declarations of '" <> convert_str name <> "'")
            (map (\ at -> (decl_at_span at, Diagnostic.MsgError, decl_at_message name at)) decl_ats)
            []
        where
            decl_at_span (DeclAt sp) = Just sp
            decl_at_span ImplicitPrim = Nothing
            decl_at_message _ (DeclAt _) = Nothing
            decl_at_message n ImplicitPrim = Just $ "'" <> convert_str n <> "' is implicitly declared as a primitive" -- TODO: reword this message (ideally when it is declared through the prelude import the message would be something like 'implicitly declared by implicit import of prelude')

    to_error (PathInPattern (Located sp _)) = Diagnostic.Error Codes.binding_lhs_path (Just sp) "path in pattern" [] []

    to_error (PathInTypeName (Located sp _)) = Diagnostic.Error Codes.path_in_type_name (Just sp) "path in type name" [] []

    to_error (PathInVariantName (Located sp _)) = Diagnostic.Error Codes.path_in_variant_name (Just sp) "path in 'data' variant name" [] []

    to_error (PathInFieldName (Located sp _)) = Diagnostic.Error Codes.path_in_field_name (Just sp) "path in field name" [] []

    to_error (Tuple1 sp) = Diagnostic.Error Codes.tuple1 (Just sp) "tuple of 1 element" [] []

    to_error (Tuple0 sp) = Diagnostic.Error Codes.tuple0 (Just sp) "tuple of 0 elements" [] []

type HIR = HIR.HIR Identifier TypeExpr () ()

type Identifier = (HIR.NameContext, [Located Text])
type Decl = HIR.Decl Identifier TypeExpr () ()
type Binding = HIR.Binding Identifier TypeExpr () ()
type ADT = Type.ADT TypeExpr
type TypeSynonym = Type.TypeSynonym TypeExpr
type TypeExpr = HIR.TypeExpr Identifier
type Expr = HIR.Expr Identifier TypeExpr () ()
type Pattern = HIR.Pattern Identifier ()
type BoundValue = HIR.BoundValue ()

type DeclChildrenList = [(Text, DeclAt, DeclKey)]
type BoundValueList = [(Text, DeclAt, BoundValueKey)]

type DeclArena = Arena.Arena Decl DeclKey
type ADTArena = Arena.Arena ADT ADTKey
type TypeSynonymArena = Arena.Arena TypeSynonym TypeSynonymKey
type BoundValueArena = Arena.Arena BoundValue BoundValueKey

type MakeIRState = StateT (DeclArena, ADTArena, TypeSynonymArena, BoundValueArena) (IDGen.IDGenT ID.ExprID (Compiler.WithDiagnostics Error Void))

new_decl :: Decl -> MakeIRState DeclKey
new_decl d =
    state $ \ (decls, adts, type_synonyms, bound_values) ->
        let (key, decls') = Arena.put d decls
        in (key, (decls', adts, type_synonyms, bound_values))

new_bound_value :: ID.BoundValueID -> Span -> MakeIRState BoundValueKey
new_bound_value id sp =
    state $ \ (decls, adts, type_synonyms, bound_values) ->
        let (key, bound_values') = Arena.put (HIR.BoundValue id () sp) bound_values
        in (key, (decls, adts, type_synonyms, bound_values'))

new_adt :: ADT -> MakeIRState ADTKey
new_adt adt =
    state $ \ (decls, adts, type_synonyms, bound_values) ->
        let (key, adts') = Arena.put adt adts
        in (key, (decls, adts', type_synonyms, bound_values))

new_type_synonym :: TypeSynonym -> MakeIRState Type.TypeSynonymKey
new_type_synonym ts =
    state $ \ (decls, adts, type_synonyms, bound_values) ->
        let (key, type_synonyms') = Arena.put ts type_synonyms
        in (key, (decls, adts, type_synonyms', bound_values))

tell_error :: Error -> MakeIRState ()
tell_error = lift . lift . Compiler.tell_error

new_expr_id :: MakeIRState ID.ExprID
new_expr_id = lift $ IDGen.gen_id

make_name_context :: DeclChildrenList -> BoundValueList -> Maybe HIR.NameContext -> MakeIRState HIR.NameContext
make_name_context decls bound_values parent =
    let decl_dups = find_dups decls
        bn_dups = find_dups bound_values
    in report_dups decl_dups >> report_dups bn_dups >>
    pure (HIR.NameContext (make_map decls) (make_map bound_values) parent)
    where
        -- separate finding duplicates from making maps so that if there is a duplicate the whole name contexet doesnt just disappear
        -- duplicates will just take the last bound name in the last, because of the how Map.fromList is implemented
        find_dups x =
            let grouped = List.groupBy ((==) `on` get_name) $ List.sortBy (compare `on` get_name) x -- compare names of bound names only
            in filter ((1/=) . length) grouped
            where
                get_name (n, _, _) = n

        make_map x = Map.fromList (map (\ (n, _, v) -> (n, v)) x)

        report_dups = mapM_ (\ decls@((first_name, _, _):_) -> tell_error $ MultipleDecls first_name (map get_decl_at decls))
            where
                get_decl_at (_, d, _) = d

make_iden1 :: Located [Located Text] -> Maybe (Located Text)
make_iden1 (Located _ [iden1]) = Just iden1
make_iden1 _ = Nothing

make_iden1_with_err :: (Located [Located Text] -> Error) -> Located [Located Text] -> MakeIRState (Maybe (Located Text))
make_iden1_with_err make_err iden =
    case make_iden1 iden of
        Just res -> pure $ Just res
        Nothing -> tell_error (make_err iden) >> pure Nothing

primitive_decls :: MakeIRState DeclChildrenList
primitive_decls =
    new_decl (HIR.Decl'Type Type.Type'Int) >>= \ int ->
    new_decl (HIR.Decl'Type Type.Type'Float) >>= \ float ->
    new_decl (HIR.Decl'Type Type.Type'Char) >>= \ char ->
    new_decl (HIR.Decl'Type Type.Type'String) >>= \ string ->
    new_decl (HIR.Decl'Type Type.Type'Bool) >>= \ bool ->
    pure
        [ ("int", ImplicitPrim, int)
        , ("float", ImplicitPrim, float)
        , ("char", ImplicitPrim, char)
        , ("string", ImplicitPrim, string)
        , ("bool", ImplicitPrim, bool)
        ]
primitive_values :: MakeIRState BoundValueList
primitive_values = pure []

convert :: [AST.Decl] -> Compiler.WithDiagnostics Error Void HIR
convert decls =
    IDGen.run_id_gen_t ID.ExprID'HIRGen $
        runStateT
            (
                let module_id = ID.ModuleID [] -- TODO: figure out how the module system is going to work
                in primitive_decls >>= \ primitive_decls ->
                primitive_values >>= \ primitive_values ->
                convert_decls (ID.BVParent'Module module_id) (ID.DeclParent'Module module_id) Nothing primitive_decls primitive_values decls >>= \ (name_context, bindings, adts, type_synonyms) ->
                new_decl (HIR.Decl'Module module_id name_context bindings adts type_synonyms)
            )
            (Arena.new, Arena.new, Arena.new, Arena.new) >>= \ (mod, (decls, adts, type_synonyms, bound_values)) ->
        pure (HIR.HIR decls adts type_synonyms bound_values mod)

convert_decls :: ID.BoundValueParent -> ID.DeclParent -> Maybe HIR.NameContext -> DeclChildrenList -> BoundValueList -> [AST.Decl] -> MakeIRState (HIR.NameContext, [Binding], [ADTKey], [TypeSynonymKey])
convert_decls bv_parent decl_parent parent_name_context prev_decl_entries prev_bv_entries decls =
    mfix (\ ~(final_name_context, _, _, _) -> -- mfix needed because the name context to put the expressions into is this one
        List.unzip5 <$> zipWithM (convert_decl final_name_context) [0..] decls >>= \ (decl_entries, bound_value_entries, bindings, adts, type_synonyms) ->
        make_name_context (prev_decl_entries ++ concat decl_entries) (prev_bv_entries ++ concat bound_value_entries) parent_name_context >>= \ name_context ->
        pure (name_context, concat bindings, concat adts, concat type_synonyms)
    )
    where
        convert_decl final_name_context ind (AST.Decl'Value target eq_sp expr) =
            convert_expr final_name_context expr >>= \ expr' ->
            convert_pattern (ID.PatID (ID.PatParent'Binding decl_parent ind) []) bv_parent target >>= \ (new_bound_values, target') ->
            let binding = HIR.Binding target' eq_sp expr'
            in pure ([], new_bound_values, [binding], [], [])

        convert_decl final_name_context _ (AST.Decl'Data name variants) =
            runMaybeT (
                iden1_for_type_name name >>= \ (Located name1sp name1) ->
                Type.ADT (ID.DeclID decl_parent name1) name1 <$> mapM (convert_variant final_name_context) variants >>= \ datatype ->

                lift (new_adt datatype) >>= \ adt_key ->
                -- TODO: add constructors to bound name table
                lift (new_decl (HIR.Decl'Type $ Type.Type'ADT adt_key)) >>= \ decl_key ->

                pure (name1, DeclAt name1sp, decl_key, adt_key)
            ) >>= \case
                Just (name1, decl_at, decl_key, adt_key) -> pure ([(name1, decl_at, decl_key)], [], [], [adt_key], [])
                Nothing -> pure ([], [], [], [], [])

        convert_decl final_name_context _ (AST.Decl'TypeSyn name expansion) =
            runMaybeT (
                lift (convert_type final_name_context expansion) >>= \ expansion' ->
                iden1_for_type_name name >>= \ (Located name1sp name1) ->
                lift (new_type_synonym (Type.TypeSynonym (ID.DeclID decl_parent name1) name1 expansion')) >>= \ syn_key ->
                lift (new_decl (HIR.Decl'Type $ Type.Type'Synonym syn_key)) >>= \ decl_key ->

                pure (name1, DeclAt name1sp, decl_key, syn_key)
            ) >>= \case
                Just (name1, decl_at, decl_key, syn_key) -> pure ([(name1, decl_at, decl_key)], [], [], [], [syn_key])
                Nothing -> pure ([], [], [], [], [])

        iden1_for_variant_name = MaybeT . make_iden1_with_err PathInVariantName
        iden1_for_type_name = MaybeT . make_iden1_with_err PathInTypeName
        iden1_for_field_name = MaybeT . make_iden1_with_err PathInFieldName

        convert_variant name_context (AST.DataVariant'Anon name fields) = Type.ADTVariant'Anon <$> (unlocate <$> iden1_for_variant_name name) <*> lift (mapM (convert_type name_context) fields)
        convert_variant name_context (AST.DataVariant'Named name fields) =
            -- TOOD: making getter functions
            Type.ADTVariant'Named
                <$> (unlocate <$> iden1_for_variant_name name)
                <*> mapM
                    (\ (field_name, ty_ast) ->
                        (,)
                            <$> (unlocate <$> iden1_for_field_name field_name)
                            <*> lift (convert_type name_context ty_ast))
                    fields

convert_type :: HIR.NameContext -> AST.Type -> MakeIRState TypeExpr
convert_type nc (AST.Type'Identifier id) = pure $ HIR.TypeExpr'Identifier (just_span id) (nc, unlocate id)
convert_type nc (AST.Type'Tuple sp items) = mapM (convert_type nc) items >>= group_items
    where
        group_items [a, b] = pure $ HIR.TypeExpr'Tuple a b
        group_items (a:b:more) = HIR.TypeExpr'Tuple a <$> group_items (b:more)
        group_items [_] = tell_error (Tuple1 sp) >> pure (HIR.TypeExpr'Poison sp)
        group_items [] = tell_error (Tuple0 sp) >> pure (HIR.TypeExpr'Poison sp)

convert_expr :: HIR.NameContext -> AST.Expr -> MakeIRState Expr
convert_expr nc (AST.Expr'Identifier iden) = new_expr_id >>= \ id -> pure (HIR.Expr'Identifier id () (just_span iden) (nc, unlocate iden))
convert_expr _ (AST.Expr'Char sp c) = new_expr_id >>= \ id -> pure (HIR.Expr'Char id () sp c)
convert_expr _ (AST.Expr'String sp s) = new_expr_id >>= \ id -> pure (HIR.Expr'String id () sp s)
convert_expr _ (AST.Expr'Int sp i) = new_expr_id >>= \ id -> pure (HIR.Expr'Int id () sp i)
convert_expr _ (AST.Expr'Float sp f) = new_expr_id >>= \ id -> pure (HIR.Expr'Float id () sp f)
convert_expr _ (AST.Expr'Bool sp b) = new_expr_id >>= \ id -> pure (HIR.Expr'Bool id () sp b)

convert_expr name_context (AST.Expr'Tuple sp items) =
    zipWithM (\ i -> convert_expr name_context) [0..] items >>= group_items
    where
        group_items [a, b] = new_expr_id >>= \ id -> pure (HIR.Expr'Tuple id () sp a b)
        group_items (a:b:more) = new_expr_id >>= \ id -> HIR.Expr'Tuple id () sp a <$> group_items (b:more) -- TODO: properly do span of b:more because this just takes the span of the whole thing
        group_items [_] = tell_error (Tuple1 sp) >> new_expr_id >>= \ id -> pure (HIR.Expr'Poison id () sp)
        group_items [] = tell_error (Tuple0 sp) >> new_expr_id >>= \ id -> pure (HIR.Expr'Poison id () sp)

convert_expr name_context (AST.Expr'Lambda sp params body) = convert_lambda name_context (zip [0..] params) body
    where
        convert_lambda name_context ((param_i, param):more) body =
            new_expr_id >>= \ id ->
            convert_pattern (ID.PatID (ID.PatParent'LambdaParam id param_i) []) (ID.BVParent'LambdaParam id) param >>= \ (bound_value_list, param) ->
            make_name_context [] bound_value_list (Just name_context) >>= \ lambda_nc ->
            HIR.Expr'Lambda id () sp param <$> convert_lambda lambda_nc more body -- TODO: properly do spans of parts because this also just takes the whole span

        convert_lambda name_context [] body = convert_expr name_context body

convert_expr name_context (AST.Expr'Let sp decls subexpr) = go name_context decls
    where
        go parent [] = convert_expr parent subexpr
        go parent (first:more) =
            new_expr_id >>= \ id ->
            convert_decls (ID.BVParent'Let id) (ID.DeclParent'Expr id) (Just parent) [] [] [first] >>= \ (let_context, bindings, _, _) -> -- TODO: put adts and type synonyms into module
            HIR.Expr'Let id () sp bindings <$> go let_context more
convert_expr name_context (AST.Expr'LetRec sp decls subexpr) =
    new_expr_id >>= \ id ->
    convert_decls (ID.BVParent'Let id) (ID.DeclParent'Expr id) (Just name_context) [] [] decls >>= \ (let_context, bindings, _, _) -> -- TODO: put adts and type synonyms into module
    HIR.Expr'Let id () sp bindings <$> convert_expr let_context subexpr

convert_expr name_context (AST.Expr'BinaryOps sp first ops) = new_expr_id >>= \ id -> HIR.Expr'BinaryOps id () () sp <$> convert_expr name_context first <*> zipWithM (\ ind (op, right) -> convert_expr name_context right >>= \ right' -> pure ((name_context, unlocate op), right')) [1..] ops

convert_expr name_context (AST.Expr'Call sp callee args) =
    convert_expr name_context callee >>= \ callee ->
    foldlM (\ callee (arg_i, arg) -> new_expr_id >>= \ id -> HIR.Expr'Call id () sp callee <$> convert_expr name_context arg) callee (zip [0..] args)

convert_expr name_context (AST.Expr'If sp if_sp cond t f) = new_expr_id >>= \ id -> HIR.Expr'If id () sp if_sp <$> convert_expr name_context cond <*> convert_expr name_context t <*> convert_expr name_context f
convert_expr name_context (AST.Expr'Case sp case_sp e arms) =
    new_expr_id >>= \ id ->
    convert_expr name_context e >>= \ e ->
    zipWithM
        (\ ind (pat, choice) ->
            convert_pattern (ID.PatID (ID.PatParent'CaseArm id ind) []) (ID.BVParent'CaseArm id ind) pat >>= \ (new_bound_values, pat) ->
            make_name_context [] new_bound_values (Just name_context) >>= \ arm_nc ->
            convert_expr arm_nc choice >>= \ choice ->
            pure (pat, choice))
        [0..]
        arms
        >>= \ arms ->
    pure (HIR.Expr'Case id () sp case_sp e arms)

convert_expr nc (AST.Expr'TypeAnnotation sp ty e) = new_expr_id >>= \ id -> HIR.Expr'TypeAnnotation id () sp <$> convert_type nc ty <*> convert_expr nc e

convert_pattern :: ID.PatID -> ID.BoundValueParent -> AST.Pattern -> MakeIRState (BoundValueList, Pattern)
convert_pattern id parent (AST.Pattern'Identifier iden) =
    make_iden1_with_err PathInPattern iden >>= \case
        Just (Located name_sp name) ->
            new_bound_value (ID.BoundValueID parent name) name_sp >>= \ bn ->
            pure ([(name, DeclAt name_sp, bn)], HIR.Pattern'Identifier id () name_sp bn)

        Nothing -> pure ([], HIR.Pattern'Poison id () (just_span iden))
convert_pattern id _ (AST.Pattern'Wildcard sp) = pure ([], HIR.Pattern'Wildcard id () sp)
convert_pattern id parent (AST.Pattern'Tuple sp subpats) =
    List.unzip <$> zipWithM (\ ind -> convert_pattern id parent) [0..] subpats >>= \ (bound_values, subpats') ->
    go id subpats' >>= \ subpats_grouped ->
    pure (concat bound_values, subpats_grouped)
    where
        go id [a, b] = pure $ HIR.Pattern'Tuple id () sp a b
        go id (a:b:more) = HIR.Pattern'Tuple id () sp a <$> go id (b:more)
        go id [_] = tell_error (Tuple1 sp) >> pure (HIR.Pattern'Poison id () sp)
        go id [] = tell_error (Tuple0 sp) >> pure (HIR.Pattern'Poison id () sp)
convert_pattern id parent (AST.Pattern'Named sp iden at_sp subpat) =
    convert_pattern id parent subpat >>= \ (sub_bn, subpat') ->
    make_iden1_with_err PathInPattern iden >>= \case
        Just (Located name_sp name) ->
            new_bound_value (ID.BoundValueID parent name) name_sp >>= \ bn ->
            pure ((name, DeclAt name_sp, bn) : sub_bn, HIR.Pattern'Named id () sp at_sp (Located name_sp bn) subpat')

        Nothing ->
            pure ([], HIR.Pattern'Poison id () sp)
