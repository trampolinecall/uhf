module UHF.ASTToIR (convert) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.AST as AST
import qualified UHF.HIR as HIR

import UHF.IO.Span (Span)
import UHF.IO.Located (Located (..))

import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Codes

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe

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

data DeclAt = DeclAt Span | ImplicitPrim

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

type Identifier = (HIR.NameContext, [Located Text])
type Decl = HIR.Decl
type Binding = HIR.Binding Identifier TypeExpr () ()
type NominalType = HIR.NominalType TypeExpr
type TypeExpr = HIR.TypeExpr Identifier
type Expr = HIR.Expr Identifier TypeExpr () ()
type Pattern = HIR.Pattern Identifier ()

type DeclArena = Arena.Arena Decl HIR.DeclKey
type BindingArena = Arena.Arena Binding HIR.BindingKey
type BoundValueArena = Arena.Arena (HIR.BoundValue ()) HIR.BoundValueKey
type NominalTypeArena = Arena.Arena NominalType HIR.NominalTypeKey

type DeclChildrenList = [(Text, DeclAt, HIR.DeclKey)]
type BoundValueList = [(Text, DeclAt, HIR.BoundValueKey)]

type MakeIRState = StateT (DeclArena, BindingArena, BoundValueArena, NominalTypeArena) (Writer [Error])

new_decl :: Decl -> MakeIRState HIR.DeclKey
new_decl d =
    state $ \ (decls, bindings, bound_values, nominals) ->
        let (key, decls') = Arena.put d decls
        in (key, (decls', bindings, bound_values, nominals))

new_binding :: Binding -> MakeIRState HIR.BindingKey
new_binding v =
    state $ \ (decls, bindings, bound_values, nominals) ->
        let (key, bindings') = Arena.put v bindings
        in (key, (decls, bindings', bound_values, nominals))

new_bound_value :: Text -> Span -> MakeIRState HIR.BoundValueKey
new_bound_value _ sp =
    state $ \ (decls, bindings, bound_values, nominals) ->
        let (key, bound_values') = Arena.put (HIR.BoundValue () sp) bound_values
        in (key, (decls, bindings, bound_values', nominals))

new_nominal_type :: NominalType -> MakeIRState HIR.NominalTypeKey
new_nominal_type nominal =
    state $ \ (decls, bindings, bound_values, nominals) ->
        let (key, nominals') = Arena.put nominal nominals
        in (key, (decls, bindings, bound_values, nominals'))

tell_err :: Error -> MakeIRState ()
tell_err = lift . tell . (:[])

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
            let grouped = List.groupBy ((==) `on` get_name) $ List.sortBy (compare `on` get_name) x -- compare names of bindings only
            in filter ((1/=) . length) grouped
            where
                get_name (n, _, _) = n

        make_map x = Map.fromList (map (\ (n, _, v) -> (n, v)) x)

        report_dups = mapM_ (\ decls@((first_name, _, _):_) -> tell_err $ MultipleDecls first_name (map get_decl_at decls))
            where
                get_decl_at (_, d, _) = d

make_iden1 :: Located [Located Text] -> Maybe (Located Text)
make_iden1 (Located _ [iden1]) = Just iden1
make_iden1 _ = Nothing

make_iden1_with_err :: (Located [Located Text] -> Error) -> Located [Located Text] -> MakeIRState (Maybe (Located Text))
make_iden1_with_err make_err iden =
    case make_iden1 iden of
        Just res -> pure $ Just res
        Nothing -> tell_err (make_err iden) >> pure Nothing

primitive_decls :: MakeIRState DeclChildrenList
primitive_decls =
    new_decl (HIR.Decl'Type HIR.Type'Int) >>= \ int ->
    new_decl (HIR.Decl'Type HIR.Type'Float) >>= \ float ->
    new_decl (HIR.Decl'Type HIR.Type'Char) >>= \ char ->
    new_decl (HIR.Decl'Type HIR.Type'String) >>= \ string ->
    new_decl (HIR.Decl'Type HIR.Type'Bool) >>= \ bool ->
    pure
        [ ("int", ImplicitPrim, int)
        , ("float", ImplicitPrim, float)
        , ("char", ImplicitPrim, char)
        , ("string", ImplicitPrim, string)
        , ("bool", ImplicitPrim, bool)
        ]
primitive_values :: MakeIRState BoundValueList
primitive_values = pure []

convert :: [AST.Decl] -> Compiler.Compiler (DeclArena, NominalTypeArena, BindingArena, BoundValueArena)
convert decls =
    let -- prim_span = Span.start_of_file file TODO: remove this function
        (res, errs) = runWriter (
                runStateT
                    (
                        primitive_decls >>= \ primitive_decls ->
                        primitive_values >>= \ primitive_values ->
                        HIR.Decl'Module <$> convert_decls Nothing primitive_decls primitive_values decls >>= new_decl
                    )
                    (Arena.new, Arena.new, Arena.new, Arena.new) >>= \ (_, (decls, bindings, bound_values, nominals)) ->
                pure (decls, nominals, bindings, bound_values)
            )
    in Compiler.errors errs >> pure res

convert_decls :: Maybe HIR.NameContext -> DeclChildrenList -> BoundValueList -> [AST.Decl] -> MakeIRState HIR.NameContext
convert_decls parent_context prev_decl_entries prev_bv_entries decls =
    mfix (\ final_name_context -> -- mfix needed because the name context to put the expressions into is this one
        List.unzip <$> mapM
            (\case
                AST.Decl'Value target eq_sp expr ->
                    convert_expr final_name_context expr >>= \ expr' ->
                    convert_pattern target >>= \ (new_bound_values, target') ->
                    new_binding (HIR.Binding target' eq_sp expr') >>
                    pure ([], new_bound_values)

                AST.Decl'Data name variants ->
                    runMaybeT (
                        iden1_for_type_name name >>= \ (Located name1sp name1) ->
                        HIR.NominalType'Data name1 <$> mapM (convert_variant final_name_context) variants >>= \ datatype ->

                        lift (new_nominal_type datatype) >>= \ nominal_type_key ->
                        -- TODO: add constructors to bound name table
                        lift (new_decl (HIR.Decl'Type $ HIR.Type'Nominal nominal_type_key)) >>= \ decl_key ->

                        pure (name1, DeclAt name1sp, decl_key)
                    ) >>= \ new_decl_entry ->
                    pure (Maybe.maybeToList new_decl_entry, [])

                AST.Decl'TypeSyn name expansion ->
                    runMaybeT (
                        lift (convert_type final_name_context expansion) >>= \ expansion' ->
                        iden1_for_type_name name >>= \ (Located name1sp name1) ->
                        lift (new_nominal_type (HIR.NominalType'Synonym name1 expansion')) >>= \ nominal_type_key ->
                        lift (new_decl (HIR.Decl'Type $ HIR.Type'Nominal nominal_type_key)) >>= \ decl_key ->

                        pure (name1, DeclAt name1sp, decl_key)
                    ) >>= \ new_decl_entry ->
                    pure (Maybe.maybeToList new_decl_entry, [])
            )
            decls >>= \ (decl_entries, bound_value_entries) ->
        make_name_context (prev_decl_entries ++ concat decl_entries) (prev_bv_entries ++ concat bound_value_entries) parent_context)
    where
        iden1_for_variant_name = MaybeT . make_iden1_with_err PathInVariantName
        iden1_for_type_name = MaybeT . make_iden1_with_err PathInTypeName
        iden1_for_field_name = MaybeT . make_iden1_with_err PathInFieldName

        convert_variant name_context (AST.DataVariant'Anon name fields) = HIR.DataVariant'Anon <$> (unlocate <$> iden1_for_variant_name name) <*> lift (mapM (convert_type name_context) fields)
        convert_variant name_context (AST.DataVariant'Named name fields) =
            -- TOOD: making getter functions
            HIR.DataVariant'Named
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
        group_items [_] = tell_err (Tuple1 sp) >> pure (HIR.TypeExpr'Poison sp)
        group_items [] = tell_err (Tuple0 sp) >> pure (HIR.TypeExpr'Poison sp)

convert_expr :: HIR.NameContext -> AST.Expr -> MakeIRState Expr
convert_expr nc (AST.Expr'Identifier iden) = pure $ HIR.Expr'Identifier () (just_span iden) (nc, unlocate iden)
convert_expr _ (AST.Expr'Char sp c) = pure $ HIR.Expr'Char () sp c
convert_expr _ (AST.Expr'String sp s) = pure $ HIR.Expr'String () sp s
convert_expr _ (AST.Expr'Int sp i) = pure $ HIR.Expr'Int () sp i
convert_expr _ (AST.Expr'Float sp f) = pure $ HIR.Expr'Float () sp f
convert_expr _ (AST.Expr'Bool sp b) = pure $ HIR.Expr'Bool () sp b

convert_expr parent_context (AST.Expr'Tuple sp items) =
    mapM (convert_expr parent_context) items >>= group_items
    where
        group_items [a, b] = pure $ HIR.Expr'Tuple () sp a b
        group_items (a:b:more) = HIR.Expr'Tuple () sp a <$> group_items (b:more) -- TODO: properly do span of b:more because this just takes the span of the whole thing
        group_items [_] = tell_err (Tuple1 sp) >> pure (HIR.Expr'Poison () sp)
        group_items [] = tell_err (Tuple0 sp) >> pure (HIR.Expr'Poison () sp)

convert_expr parent_context (AST.Expr'Lambda sp params body) = convert_lambda parent_context params body
    where
        convert_lambda parent_context (param:more) body =
            convert_pattern param >>= \ (bound_value_list, param) ->
            make_name_context [] bound_value_list (Just parent_context) >>= \ lambda_nc ->
            HIR.Expr'Lambda () sp param <$> convert_lambda lambda_nc more body -- TODO: properly do spans of parts because this also just takes the whole span

        convert_lambda parent_context [] body = convert_expr parent_context body

convert_expr parent_context (AST.Expr'Let sp decls subexpr) =
    convert_decls (Just parent_context) [] [] decls >>= \ let_context ->
    HIR.Expr'Let () sp <$> convert_expr let_context subexpr -- TODO: actually do sequentially because convert_decls does all at once
convert_expr parent_context (AST.Expr'LetRec sp decls subexpr) =
    convert_decls (Just parent_context) [] [] decls >>= \ let_context ->
    HIR.Expr'LetRec () sp <$> convert_expr let_context subexpr

convert_expr parent_context (AST.Expr'BinaryOps sp first ops) = HIR.Expr'BinaryOps () () sp <$> convert_expr parent_context first <*> mapM (\ (op, right) -> convert_expr parent_context right >>= \ right' -> pure ((parent_context, unlocate op), right')) ops

convert_expr parent_context (AST.Expr'Call sp callee args) =
    convert_expr parent_context callee >>= \ callee ->
    foldlM (\ callee arg -> HIR.Expr'Call () sp callee <$> convert_expr parent_context arg) callee args

convert_expr parent_context (AST.Expr'If sp if_sp cond t f) = HIR.Expr'If () sp if_sp <$> convert_expr parent_context cond <*> convert_expr parent_context t <*> convert_expr parent_context f
convert_expr parent_context (AST.Expr'Case sp case_sp e arms) =
    convert_expr parent_context e >>= \ e ->
    mapM
        (\ (pat, choice) ->
            convert_pattern pat >>= \ (new_bound_values, pat) ->
            make_name_context [] new_bound_values (Just parent_context) >>= \ arm_nc ->
            convert_expr arm_nc choice >>= \ choice ->
            pure (pat, choice))
        arms
        >>= \ arms ->
    pure (HIR.Expr'Case () sp case_sp e arms)

convert_expr nc (AST.Expr'TypeAnnotation sp ty e) = HIR.Expr'TypeAnnotation () sp <$> convert_type nc ty <*> convert_expr nc e

convert_pattern :: AST.Pattern -> MakeIRState (BoundValueList, Pattern)
convert_pattern (AST.Pattern'Identifier iden) =
    make_iden1_with_err PathInPattern iden >>= \case
        Just (Located name_sp name) ->
            new_bound_value name name_sp >>= \ bn ->
            pure ([(name, DeclAt name_sp, bn)], HIR.Pattern'Identifier () name_sp bn)

        Nothing -> pure ([], HIR.Pattern'Poison () (just_span iden))
convert_pattern (AST.Pattern'Wildcard sp) = pure ([], HIR.Pattern'Wildcard () sp)
convert_pattern (AST.Pattern'Tuple sp subpats) =
    List.unzip <$> mapM convert_pattern subpats >>= \ (bound_values, subpats') ->
    go subpats' >>= \ subpats_grouped ->
    pure (concat bound_values, subpats_grouped)
    where
        go [a, b] = pure $ HIR.Pattern'Tuple () sp a b
        go (a:b:more) = HIR.Pattern'Tuple () sp a <$> go (b:more)
        go [_] = tell_err (Tuple1 sp) >> pure (HIR.Pattern'Poison () sp)
        go [] = tell_err (Tuple0 sp) >> pure (HIR.Pattern'Poison () sp)
convert_pattern (AST.Pattern'Named sp iden at_sp subpat) =
    convert_pattern subpat >>= \ (sub_bn, subpat') ->
    make_iden1_with_err PathInPattern iden >>= \case
        Just (Located name_sp name) ->
            new_bound_value name name_sp >>= \ bn ->
            pure ((name, DeclAt name_sp, bn) : sub_bn, HIR.Pattern'Named () sp at_sp (Located name_sp bn) subpat')

        Nothing ->
            pure ([], HIR.Pattern'Poison () sp)
