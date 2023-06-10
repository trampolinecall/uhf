module UHF.Phases.Middle.ToSIR (convert) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.Data.AST as AST
import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.IR.IDGen as IDGen

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

type SIR = SIR.SIR Identifier Identifier Identifier () ()

type Identifier = (SIR.NameContext, [Located Text])
type Decl = SIR.Decl Identifier Identifier Identifier () ()
type Binding = SIR.Binding Identifier Identifier Identifier () ()
type ADT = Type.ADT TypeExpr
type TypeSynonym = Type.TypeSynonym TypeExpr
type TypeExpr = SIR.TypeExpr Identifier ()
type Expr = SIR.Expr Identifier Identifier Identifier () ()
type Pattern = SIR.Pattern Identifier ()
type BoundValue = SIR.BoundValue ()

type DeclChildrenList = [(Text, DeclAt, SIR.DeclKey)]
type BoundValueList = [(Text, DeclAt, SIR.BoundValueKey)]
type ADTVariantList = [(Text, DeclAt, Type.ADTVariantIndex)]

type DeclArena = Arena.Arena Decl SIR.DeclKey
type ADTArena = Arena.Arena ADT Type.ADTKey
type TypeSynonymArena = Arena.Arena TypeSynonym Type.TypeSynonymKey
type BoundValueArena = Arena.Arena BoundValue SIR.BoundValueKey
type TypeVarArena = Arena.Arena Type.Var Type.TypeVarKey

type MakeIRState = StateT (DeclArena, ADTArena, TypeSynonymArena, TypeVarArena, BoundValueArena) (IDGen.IDGenT ID.ExprID (Compiler.WithDiagnostics Error Void))

new_decl :: Decl -> MakeIRState SIR.DeclKey
new_decl d =
    state $ \ (decls, adts, type_synonyms, type_vars, bound_values) ->
        let (key, decls') = Arena.put d decls
        in (key, (decls', adts, type_synonyms, type_vars, bound_values))

new_adt :: ADT -> MakeIRState Type.ADTKey
new_adt adt =
    state $ \ (decls, adts, type_synonyms, type_vars, bound_values) ->
        let (key, adts') = Arena.put adt adts
        in (key, (decls, adts', type_synonyms, type_vars, bound_values))

new_type_synonym :: TypeSynonym -> MakeIRState Type.TypeSynonymKey
new_type_synonym ts =
    state $ \ (decls, adts, type_synonyms, type_vars, bound_values) ->
        let (key, type_synonyms') = Arena.put ts type_synonyms
        in (key, (decls, adts, type_synonyms', type_vars, bound_values))

new_type_var :: Text -> MakeIRState Type.TypeVarKey
new_type_var name =
    state $ \ (decls, adts, type_synonyms, type_vars, bound_values) ->
        let (key, type_vars') = Arena.put (Type.Var name) type_vars
        in (key, (decls, adts, type_synonyms, type_vars', bound_values))

new_bound_value :: BoundValue -> MakeIRState SIR.BoundValueKey
new_bound_value bv =
    state $ \ (decls, adts, type_synonyms, type_vars, bound_values) ->
        let (key, bound_values') = Arena.put bv bound_values
        in (key, (decls, adts, type_synonyms, type_vars, bound_values'))

tell_error :: Error -> MakeIRState ()
tell_error = lift . lift . Compiler.tell_error

new_expr_id :: MakeIRState ID.ExprID
new_expr_id = lift IDGen.gen_id

make_name_context :: DeclChildrenList -> BoundValueList -> ADTVariantList -> Maybe SIR.NameContext -> MakeIRState SIR.NameContext
make_name_context decls bound_values adt_variants parent =
    let decl_dups = find_dups decls
        bn_dups = find_dups bound_values
    in report_dups decl_dups >> report_dups bn_dups >>
    pure (SIR.NameContext (make_map decls) (make_map bound_values) (make_map adt_variants) parent)
    where
        -- separate finding duplicates from making maps so that if there is a duplicate the whole name contexet doesnt just disappear
        -- duplicates will just take the last bound name in the last, because of the how Map.fromList is implemented
        find_dups x =
            let grouped = List.groupBy ((==) `on` get_name) $ List.sortBy (compare `on` get_name) x -- compare names of bound names only
            in filter ((1/=) . length) grouped
            where
                get_name (n, _, _) = n

        make_map x = Map.fromList (map (\ (n, _, v) -> (n, v)) x)

        report_dups = mapM_
            (\ decls ->
                let (first_name, _, _) = head decls
                in tell_error $ MultipleDecls first_name (map get_decl_at decls))
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
    new_decl (SIR.Decl'Type Type.Type'Int) >>= \ int ->
    new_decl (SIR.Decl'Type Type.Type'Float) >>= \ float ->
    new_decl (SIR.Decl'Type Type.Type'Char) >>= \ char ->
    new_decl (SIR.Decl'Type Type.Type'String) >>= \ string ->
    new_decl (SIR.Decl'Type Type.Type'Bool) >>= \ bool ->
    pure
        [ ("int", ImplicitPrim, int)
        , ("float", ImplicitPrim, float)
        , ("char", ImplicitPrim, char)
        , ("string", ImplicitPrim, string)
        , ("bool", ImplicitPrim, bool)
        ]
primitive_values :: MakeIRState BoundValueList
primitive_values = pure []

convert :: [AST.Decl] -> Compiler.WithDiagnostics Error Void SIR
convert decls =
    IDGen.run_id_gen_t ID.ExprID'SIRGen $
        runStateT
            (
                let module_id = ID.ModuleID [] -- TODO: figure out how the module system is going to work
                in primitive_decls >>= \ primitive_decls ->
                primitive_values >>= \ primitive_values ->
                convert_decls (ID.BVParent'Module module_id) (ID.DeclParent'Module module_id) Nothing primitive_decls primitive_values decls >>= \ (name_context, bindings, adts, type_synonyms) ->
                new_decl (SIR.Decl'Module module_id name_context bindings adts type_synonyms)
            )
            (Arena.new, Arena.new, Arena.new, Arena.new, Arena.new) >>= \ (mod, (decls, adts, type_synonyms, type_vars, bound_values)) ->
        pure (SIR.SIR decls adts type_synonyms type_vars bound_values mod)

convert_decls :: ID.BoundValueParent -> ID.DeclParent -> Maybe SIR.NameContext -> DeclChildrenList -> BoundValueList -> [AST.Decl] -> MakeIRState (SIR.NameContext, [Binding], [Type.ADTKey], [Type.TypeSynonymKey])
convert_decls bv_parent decl_parent parent_name_context prev_decl_entries prev_bv_entries decls =
    -- TODO: dont put the expressions in the final name context for non recursive lets
    mfix (\ ~(final_name_context, _, _, _) -> -- mfix needed because the name context to put the expressions into is this one
        List.unzip6 <$> mapM (convert_decl final_name_context) decls >>= \ (decl_entries, bound_value_entries, adt_variant_entries, bindings, adts, type_synonyms) ->
        make_name_context (prev_decl_entries ++ concat decl_entries) (prev_bv_entries ++ concat bound_value_entries) (concat adt_variant_entries) parent_name_context >>= \ name_context ->
        pure (name_context, concat bindings, concat adts, concat type_synonyms)
    )
    where
        -- TODO: clean this up
        convert_decl :: SIR.NameContext -> AST.Decl -> MakeIRState (DeclChildrenList, BoundValueList, ADTVariantList, [Binding], [Type.ADTKey], [Type.TypeSynonymKey])
        convert_decl final_name_context (AST.Decl'Value target eq_sp expr) =
            convert_expr final_name_context expr >>= \ expr' ->
            convert_pattern bv_parent final_name_context target >>= \ (new_bound_values, target') ->
            let binding = SIR.Binding target' eq_sp expr'
            in pure ([], new_bound_values, [], [binding], [], [])

        convert_decl final_name_context (AST.Decl'Data name type_params variants) = -- TODO: type_params
            runMaybeT (
                mapM iden1_for_type_name type_params >>= \ type_param_names ->
                mapM (lift . new_type_var . unlocate) type_param_names >>= \ ty_param_vars ->
                zipWithM (\ (Located sp name) var -> (name, DeclAt sp,) <$> lift (new_decl $ SIR.Decl'Type $ Type.Type'Variable var)) type_param_names ty_param_vars >>= \ new_decls ->
                lift (make_name_context new_decls [] [] (Just final_name_context)) >>= \ new_nc ->

                iden1_for_type_name name >>= \ (Located name1sp name1) ->
                mapM (convert_variant new_nc) variants >>= \ variants_converted ->
                let datatype = Type.ADT (ID.DeclID decl_parent name1) name1 ty_param_vars variants_converted
                in

                lift (new_adt datatype) >>= \ adt_key ->
                lift (new_decl (SIR.Decl'Type $ Type.Type'ADT adt_key [])) >>= \ decl_key ->

                unzip <$> (catMaybes <$> mapM
                    (\ case
                        (Type.ADTVariant'Anon name _, index, adt_variant_ast) ->
                            let name_sp = case adt_variant_ast of
                                    AST.DataVariant'Anon name _ -> just_span name
                                    AST.DataVariant'Named name _ -> just_span name -- technically not possible for a named ast to become an anonymous data variant but it is nice to have this not emit a warning
                            in
                            let variant_index = Type.ADTVariantIndex adt_key index
                             in lift (new_bound_value (SIR.BoundValue'ADTVariant (ID.BoundValueID bv_parent name) variant_index () name_sp)) >>= \ bv_key ->
                            pure (Just ((name, DeclAt name_sp, bv_key), SIR.Binding'ADTVariant bv_key variant_index))
                        (Type.ADTVariant'Named _ _, _, _) -> pure Nothing
                    )
                    (zip3 variants_converted [0..] variants)) >>= \ (variant_bvs, constructor_bindings) ->

                mapM
                    (\ (variant_ast, index, variant) ->
                        let variant_name = Type.variant_name variant
                            name_sp = case variant_ast of
                                AST.DataVariant'Anon name _ -> just_span name
                                AST.DataVariant'Named name _ -> just_span name
                        in pure (variant_name, DeclAt name_sp, Type.ADTVariantIndex adt_key index))
                    (zip3 variants [0..] variants_converted) >>= \ variant_entries ->

                pure (name1, DeclAt name1sp, decl_key, adt_key, variant_entries, variant_bvs, constructor_bindings)
            ) >>= \case
                Just (name1, decl_at, decl_key, adt_key, constructor_entries, constructors, constructor_bindings) -> pure ([(name1, decl_at, decl_key)], constructors, constructor_entries, constructor_bindings, [adt_key], []) -- constructors are added directly to the current namespace and are not namespaced under the type name
                Nothing -> pure ([], [], [], [], [], [])

        convert_decl final_name_context (AST.Decl'TypeSyn name expansion) =
            runMaybeT (
                lift (convert_type final_name_context expansion) >>= \ expansion' ->
                iden1_for_type_name name >>= \ (Located name1sp name1) ->
                lift (new_type_synonym (Type.TypeSynonym (ID.DeclID decl_parent name1) name1 expansion')) >>= \ syn_key ->
                lift (new_decl (SIR.Decl'Type $ Type.Type'Synonym syn_key)) >>= \ decl_key ->

                pure (name1, DeclAt name1sp, decl_key, syn_key)
            ) >>= \case
                Just (name1, decl_at, decl_key, syn_key) -> pure ([(name1, decl_at, decl_key)], [], [], [], [], [syn_key])
                Nothing -> pure ([], [], [], [], [], [])

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

convert_type :: SIR.NameContext -> AST.Type -> MakeIRState TypeExpr
convert_type nc (AST.Type'Identifier id) = pure $ SIR.TypeExpr'Identifier () (just_span id) (nc, unlocate id)
convert_type nc (AST.Type'Tuple sp items) = mapM (convert_type nc) items >>= group_items
    where
        group_items [a, b] = pure $ SIR.TypeExpr'Tuple () a b
        group_items (a:b:more) = SIR.TypeExpr'Tuple () a <$> group_items (b:more)
        group_items [_] = tell_error (Tuple1 sp) >> pure (SIR.TypeExpr'Poison () sp)
        group_items [] = tell_error (Tuple0 sp) >> pure (SIR.TypeExpr'Poison () sp)
convert_type _ (AST.Type'Hole sp id) = pure $ SIR.TypeExpr'Hole () sp id
convert_type nc (AST.Type'Function sp arg res) = SIR.TypeExpr'Function () sp <$> convert_type nc arg <*> convert_type nc res
convert_type nc (AST.Type'Forall _ tys ty) =
    catMaybes <$> mapM (make_iden1_with_err PathInTypeName) tys >>= \ tys ->

    mapM (new_type_var . unlocate) tys >>= \ ty_vars ->
    zipWithM (\ (Located sp name) var -> (name, DeclAt sp,) <$> new_decl (SIR.Decl'Type $ Type.Type'Variable var)) tys ty_vars >>= \ new_decls ->

    make_name_context new_decls [] [] (Just nc) >>= \ new_nc ->

    case ty_vars of
        [] -> convert_type new_nc ty -- can happen if there are errors in all the type names or if the user passed none
        tyv1:tyv_more -> SIR.TypeExpr'Forall () (tyv1 :| tyv_more) <$> convert_type new_nc ty

convert_type nc (AST.Type'Apply sp ty args) =
    convert_type nc ty >>= \ ty ->
    foldlM (\ ty arg -> SIR.TypeExpr'Apply () sp ty <$> convert_type nc arg) ty args -- TODO: fix spans
convert_type _ (AST.Type'Wild sp) = pure $ SIR.TypeExpr'Wild () sp

convert_expr :: SIR.NameContext -> AST.Expr -> MakeIRState Expr
convert_expr nc (AST.Expr'Identifier iden) = new_expr_id >>= \ id -> pure (SIR.Expr'Identifier id () (just_span iden) (nc, unlocate iden))
convert_expr _ (AST.Expr'Char sp c) = new_expr_id >>= \ id -> pure (SIR.Expr'Char id () sp c)
convert_expr _ (AST.Expr'String sp s) = new_expr_id >>= \ id -> pure (SIR.Expr'String id () sp s)
convert_expr _ (AST.Expr'Int sp i) = new_expr_id >>= \ id -> pure (SIR.Expr'Int id () sp i)
convert_expr _ (AST.Expr'Float sp f) = new_expr_id >>= \ id -> pure (SIR.Expr'Float id () sp f)
convert_expr _ (AST.Expr'Bool sp b) = new_expr_id >>= \ id -> pure (SIR.Expr'Bool id () sp b)

convert_expr name_context (AST.Expr'Tuple sp items) =
    mapM (convert_expr name_context)  items >>= group_items
    where
        group_items [a, b] = new_expr_id >>= \ id -> pure (SIR.Expr'Tuple id () sp a b)
        group_items (a:b:more) = new_expr_id >>= \ id -> SIR.Expr'Tuple id () sp a <$> group_items (b:more) -- TODO: properly do span of b:more because this just takes the span of the whole thing
        group_items [_] = tell_error (Tuple1 sp) >> new_expr_id >>= \ id -> pure (SIR.Expr'Poison id () sp)
        group_items [] = tell_error (Tuple0 sp) >> new_expr_id >>= \ id -> pure (SIR.Expr'Poison id () sp)

convert_expr name_context (AST.Expr'Lambda sp params body) = convert_lambda name_context params body
    where
        convert_lambda name_context (param:more) body =
            new_expr_id >>= \ id ->
            convert_pattern (ID.BVParent'LambdaParam id) name_context param >>= \ (bound_value_list, param) ->
            make_name_context [] bound_value_list [] (Just name_context) >>= \ lambda_nc ->
            SIR.Expr'Lambda id () sp param <$> convert_lambda lambda_nc more body -- TODO: properly do spans of parts because this also just takes the whole span

        convert_lambda name_context [] body = convert_expr name_context body

convert_expr name_context (AST.Expr'Let sp decls subexpr) = go name_context decls
    where
        go parent [] = convert_expr parent subexpr
        go parent (first:more) =
            new_expr_id >>= \ id ->
                -- TODO: not recursive bindings (eg `let x = x` is allowed because convert_decls puts the names into the same name context)
            convert_decls (ID.BVParent'Let id) (ID.DeclParent'Expr id) (Just parent) [] [] [first] >>= \ (let_context, bindings, _, _) -> -- TODO: put adts and type synonyms into module
            SIR.Expr'Let id () sp bindings <$> go let_context more
convert_expr name_context (AST.Expr'LetRec sp decls subexpr) =
    new_expr_id >>= \ id ->
    convert_decls (ID.BVParent'Let id) (ID.DeclParent'Expr id) (Just name_context) [] [] decls >>= \ (let_context, bindings, _, _) -> -- TODO: put adts and type synonyms into module
    SIR.Expr'Let id () sp bindings <$> convert_expr let_context subexpr

convert_expr name_context (AST.Expr'BinaryOps sp first ops) = new_expr_id >>= \ id -> SIR.Expr'BinaryOps id () () sp <$> convert_expr name_context first <*> mapM (\ (op, right) -> convert_expr name_context right >>= \ right' -> pure ((name_context, unlocate op), right')) ops

convert_expr name_context (AST.Expr'Call sp callee args) =
    convert_expr name_context callee >>= \ callee ->
    foldlM (\ callee arg -> new_expr_id >>= \ id -> SIR.Expr'Call id () sp callee <$> convert_expr name_context arg) callee args -- TODO: fix span for this

convert_expr name_context (AST.Expr'If sp if_sp cond t f) = new_expr_id >>= \ id -> SIR.Expr'If id () sp if_sp <$> convert_expr name_context cond <*> convert_expr name_context t <*> convert_expr name_context f
convert_expr name_context (AST.Expr'Case sp case_sp e arms) =
    new_expr_id >>= \ id ->
    convert_expr name_context e >>= \ e ->
    zipWithM
        (\ ind (pat, choice) ->
            convert_pattern (ID.BVParent'CaseArm id ind) name_context pat >>= \ (new_bound_values, pat) ->
            make_name_context [] new_bound_values [] (Just name_context) >>= \ arm_nc ->
            convert_expr arm_nc choice >>= \ choice ->
            pure (pat, choice))
        [0..]
        arms
        >>= \ arms ->
    pure (SIR.Expr'Case id () sp case_sp e arms)

convert_expr nc (AST.Expr'TypeAnnotation sp ty e) = new_expr_id >>= \ id -> SIR.Expr'TypeAnnotation id () sp <$> convert_type nc ty <*> convert_expr nc e
convert_expr nc (AST.Expr'Forall sp tys e) =
    catMaybes <$> mapM (make_iden1_with_err PathInTypeName) tys >>= \ tys ->

    mapM (new_type_var . unlocate) tys >>= \ ty_vars ->
    zipWithM (\ (Located sp name) var -> (name, DeclAt sp,) <$> new_decl (SIR.Decl'Type $ Type.Type'Variable var)) tys ty_vars >>= \ new_decls ->

    make_name_context new_decls [] [] (Just nc) >>= \ new_nc ->

    new_expr_id >>= \ id ->
    case ty_vars of
        [] -> convert_expr new_nc e
        tyv1:tyv_more -> SIR.Expr'Forall id () sp (tyv1 :| tyv_more) <$> convert_expr new_nc e

convert_expr nc (AST.Expr'TypeApply sp e args) =
    convert_expr nc e >>= \ e ->
    foldlM (\ e arg -> new_expr_id >>= \ id -> SIR.Expr'TypeApply id () sp e <$> convert_type nc arg) e args -- TODO: fix span for this
convert_expr _ (AST.Expr'Hole sp hid) = new_expr_id >>= \ eid -> pure (SIR.Expr'Hole eid () sp hid)

convert_pattern :: ID.BoundValueParent -> SIR.NameContext -> AST.Pattern -> MakeIRState (BoundValueList, Pattern)
convert_pattern parent _ (AST.Pattern'Identifier iden) =
    make_iden1_with_err PathInPattern iden >>= \case
        Just (Located name_sp name) ->
            new_bound_value (SIR.BoundValue (ID.BoundValueID parent name) () name_sp) >>= \ bn ->
            pure ([(name, DeclAt name_sp, bn)], SIR.Pattern'Identifier () name_sp bn)

        Nothing -> pure ([], SIR.Pattern'Poison () (just_span iden))
convert_pattern _ _ (AST.Pattern'Wildcard sp) = pure ([], SIR.Pattern'Wildcard () sp)
convert_pattern parent nc (AST.Pattern'Tuple sp subpats) =
    List.unzip <$> mapM (convert_pattern parent nc) subpats >>= \ (bound_values, subpats') ->
    go subpats' >>= \ subpats_grouped ->
    pure (concat bound_values, subpats_grouped)
    where
        go [a, b] = pure $ SIR.Pattern'Tuple () sp a b
        go (a:b:more) = SIR.Pattern'Tuple () sp a <$> go (b:more)
        go [_] = tell_error (Tuple1 sp) >> pure (SIR.Pattern'Poison () sp)
        go [] = tell_error (Tuple0 sp) >> pure (SIR.Pattern'Poison () sp)
convert_pattern parent nc (AST.Pattern'Named sp iden at_sp subpat) =
    convert_pattern parent nc subpat >>= \ (sub_bn, subpat') ->
    make_iden1_with_err PathInPattern iden >>= \case
        Just (Located name_sp name) ->
            new_bound_value (SIR.BoundValue (ID.BoundValueID parent name) () name_sp) >>= \ bn ->
            pure ((name, DeclAt name_sp, bn) : sub_bn, SIR.Pattern'Named () sp at_sp (Located name_sp bn) subpat')

        Nothing ->
            pure ([], SIR.Pattern'Poison () sp)
convert_pattern parent nc (AST.Pattern'AnonADTVariant sp iden fields) =
    unzip <$> mapM (convert_pattern parent nc) fields >>= \ (bvs, fields) ->
    pure (concat bvs, SIR.Pattern'AnonADTVariant () sp (nc, unlocate iden) fields)
convert_pattern parent nc (AST.Pattern'NamedADTVariant sp iden fields) =
    mapM (\ (field_name, field_pat) ->
        make_iden1_with_err PathInFieldName field_name >>= \case
            Just field_name ->
                convert_pattern parent nc field_pat >>= \ (bvs, field_pat) ->
                pure (Just (bvs, (field_name, field_pat)))
            Nothing -> pure Nothing
        ) fields >>= \ fields ->
    case sequence fields of
        Just fields_and_bvs ->
            let (bvs, fields) = unzip fields_and_bvs
            in pure (concat bvs, SIR.Pattern'NamedADTVariant () sp (nc, unlocate iden) fields)
        Nothing -> pure ([], SIR.Pattern'Poison () sp)
