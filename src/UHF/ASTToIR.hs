module UHF.ASTToIR
    ( Decl
    , Module
    , Binding
    , Expr
    , Pattern

    , DeclArena
    , BoundNameArena
    , BindingArena
    , NominalTypeArena

    , DeclMap
    , BoundNameMap
    , IR.NameContext

    , convert
    ) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.AST as AST
import qualified UHF.IR as IR

import qualified UHF.IO.Location as Location

import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Codes
import qualified UHF.Diagnostic.Sections.Messages as Messages

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import qualified UHF.Compiler as Compiler

import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Control.Monad.Fix (mfix)

data Error
    = MultipleDecls (Location.Located Text) [Location.Located Text]
    | PathInPattern (Location.Located [Location.Located Text]) -- TODO: make this less repetitive
    | PathInTypeName (Location.Located [Location.Located Text])
    | PathInVariantName (Location.Located [Location.Located Text])
    | PathInFieldName (Location.Located [Location.Located Text])

    | Tuple1 Location.Span
    | Tuple0 Location.Span

instance Diagnostic.IsError Error where
    to_error (MultipleDecls (Location.Located first_sp name) more) = Diagnostic.Error Codes.multiple_decls $
        Diagnostic.DiagnosticContents
            (Just first_sp)
            (show (length more + 1) <> " declarations of '" <> convert_str name <> "'")
            (map (\ (Location.Located sp _) -> (sp, Messages.Error, Nothing)) more)
            []

    to_error (PathInPattern (Location.Located sp _)) = Diagnostic.Error Codes.binding_lhs_path $ Diagnostic.DiagnosticContents (Just sp) "path in pattern" [] []

    to_error (PathInTypeName (Location.Located sp _)) = Diagnostic.Error Codes.path_in_type_name $ Diagnostic.DiagnosticContents (Just sp) "path in type name" [] []

    -- TODO: remove codes?
    to_error (PathInVariantName (Location.Located sp _)) = Diagnostic.Error Codes.path_in_variant_name $ Diagnostic.DiagnosticContents (Just sp) "path in 'data' variant name" [] []

    to_error (PathInFieldName (Location.Located sp _)) = Diagnostic.Error Codes.path_in_field_name $ Diagnostic.DiagnosticContents (Just sp) "path in field name" [] []

    to_error (Tuple1 sp) = Diagnostic.Error Codes.tuple1 $ Diagnostic.DiagnosticContents (Just sp) "tuple of 1 element" [] []

    to_error (Tuple0 sp) = Diagnostic.Error Codes.tuple0 $ Diagnostic.DiagnosticContents (Just sp) "tuple of 0 elements" [] []

type Identifier = (IR.NameContext, [Location.Located Text])
type Decl = IR.Decl
type Module = IR.Module
type Binding = IR.Binding Identifier TypeExpr () ()
type NominalType = IR.NominalType TypeExpr
type TypeExpr = IR.TypeExpr Identifier
type Expr = IR.Expr Identifier TypeExpr () ()
type Pattern = IR.Pattern Identifier ()

type DeclArena = Arena.Arena Decl IR.DeclKey
type BindingArena = Arena.Arena Binding IR.BindingKey
type BoundNameArena = Arena.Arena (IR.BoundName ()) IR.BoundNameKey
type NominalTypeArena = Arena.Arena NominalType IR.NominalTypeKey

type DeclMap = Map.Map Text IR.DeclKey
type BoundNameMap = Map.Map Text IR.BoundNameKey

type DeclChildrenList = [(Location.Located Text, IR.DeclKey)]
type BoundNameList = [(Location.Located Text, IR.BoundNameKey)]

type MakeIRState = StateT (DeclArena, BindingArena, BoundNameArena, NominalTypeArena) (Writer [Error])

new_decl :: Decl -> MakeIRState IR.DeclKey
new_decl d =
    state $ \ (decls, bindings, bound_names, nominals) ->
        let (key, decls') = Arena.put d decls
        in (key, (decls', bindings, bound_names, nominals))

new_binding :: Binding -> MakeIRState IR.BindingKey
new_binding v =
    state $ \ (decls, bindings, bound_names, nominals) ->
        let (key, bindings') = Arena.put v bindings
        in (key, (decls, bindings', bound_names, nominals))

new_bound_name :: Text -> Location.Span -> MakeIRState IR.BoundNameKey
new_bound_name _ sp =
    state $ \ (decls, bindings, bound_names, nominals) ->
        let (key, bound_names') = Arena.put (IR.BoundName () sp) bound_names
        in (key, (decls, bindings, bound_names', nominals))

new_nominal_type :: NominalType -> MakeIRState IR.NominalTypeKey
new_nominal_type nominal =
    state $ \ (decls, bindings, bound_names, nominals) ->
        let (key, nominals') = Arena.put nominal nominals
        in (key, (decls, bindings, bound_names, nominals'))

tell_err :: Error -> MakeIRState ()
tell_err = lift . tell . (:[])

make_name_context :: DeclChildrenList -> BoundNameList -> Maybe IR.NameContext -> MakeIRState IR.NameContext
make_name_context decls bound_names parent =
    let decl_dups = find_dups decls
        bn_dups = find_dups bound_names
    in report_dups decl_dups >> report_dups bn_dups >>
    pure (IR.NameContext (make_map decls) (make_map bound_names) parent)
    where
        -- separate finding duplicates from making maps so that if there is a duplicate the whole name contexet doesnt just disappear
        -- duplicates will just take the last bound name in the last, because of the how Map.fromList is implemented
        find_dups x =
            let grouped = List.groupBy ((==) `on` Location.unlocate . fst) $ List.sortBy (compare `on` Location.unlocate . fst) x -- compare names of bindings only
            in filter ((1/=) . length) grouped

        make_map x = Map.fromList (map (\ (k, v) -> (Location.unlocate k, v)) x)

        report_dups = mapM_ (\ ((first_name, _):more) -> tell_err $ MultipleDecls first_name (map fst more))

make_iden1 :: Location.Located [Location.Located Text] -> Maybe (Location.Located Text)
make_iden1 (Location.Located _ [iden1]) = Just iden1
make_iden1 _ = Nothing

make_iden1_with_err :: (Location.Located [Location.Located Text] -> Error) -> Location.Located [Location.Located Text] -> MakeIRState (Maybe (Location.Located Text))
make_iden1_with_err make_err iden =
    case make_iden1 iden of
        Just res -> pure $ Just res
        Nothing -> tell_err (make_err iden) >> pure Nothing

convert :: [AST.Decl] -> Compiler.Compiler (DeclArena, NominalTypeArena, BindingArena, BoundNameArena)
convert decls =
    let (res, errs) = runWriter (
                runStateT (IR.Decl'Module <$> (IR.Module <$> convert_decls Nothing decls) >>= new_decl) (Arena.new, Arena.new, Arena.new, Arena.new) >>= \ (_, (decls, bindings, bound_names, nominals)) ->
                pure (decls, nominals, bindings, bound_names)
            )
    in Compiler.errors errs >> pure res

convert_decls :: Maybe IR.NameContext -> [AST.Decl] -> MakeIRState IR.NameContext
convert_decls parent_context decls =
    mfix (\ final_name_context -> -- mfix needed because the name context to put the expressions into is this one
        List.unzip <$> mapM
            (\case
                AST.Decl'Value target eq_sp expr ->
                    convert_expr final_name_context expr >>= \ expr' ->
                    convert_pattern target >>= \ (new_bound_names, target') -> -- TODO: do this correctly
                    new_binding (IR.Binding target' eq_sp expr') >>
                    pure ([], new_bound_names)

                AST.Decl'Data name variants ->
                    runMaybeT (
                        iden1_for_type_name name >>= \ name1 ->
                        IR.NominalType'Data (Location.unlocate name1) <$> mapM (convert_variant final_name_context) variants >>= \ datatype ->

                        lift (new_nominal_type datatype) >>= \ nominal_type_key ->
                        -- TODO: add constructors to bound name table
                        lift (new_decl (IR.Decl'Type nominal_type_key)) >>= \ decl_key ->

                        pure (name1, decl_key)
                    ) >>= \ new_decl_entry ->
                    pure (Maybe.maybeToList new_decl_entry, [])

                AST.Decl'TypeSyn name expansion ->
                    runMaybeT (
                        lift (convert_type final_name_context expansion) >>= \ expansion' ->
                        iden1_for_type_name name >>= \ name1 ->
                        lift (new_nominal_type (IR.NominalType'Synonym (Location.unlocate name1) expansion')) >>= \ nominal_type_key ->
                        lift (new_decl (IR.Decl'Type nominal_type_key)) >>= \ decl_key ->

                        pure (name1, decl_key)
                    ) >>= \ new_decl_entry ->
                    pure (Maybe.maybeToList new_decl_entry, [])
            )
            decls >>= \ (decl_entries, bound_name_entries) ->
        make_name_context (concat decl_entries) (concat bound_name_entries) parent_context)
    where
        iden1_for_variant_name = MaybeT . make_iden1_with_err PathInVariantName
        iden1_for_type_name = MaybeT . make_iden1_with_err PathInTypeName
        iden1_for_field_name = MaybeT . make_iden1_with_err PathInFieldName

        convert_variant name_context (AST.DataVariant'Anon name fields) = IR.DataVariant'Anon <$> (Location.unlocate <$> iden1_for_variant_name name) <*> lift (mapM (convert_type name_context) fields)
        convert_variant name_context (AST.DataVariant'Named name fields) =
            -- TOOD: making getter functions
            IR.DataVariant'Named
                <$> (Location.unlocate <$> iden1_for_variant_name name)
                <*> mapM
                    (\ (field_name, ty_ast) ->
                        (,)
                            <$> (Location.unlocate <$> iden1_for_field_name field_name)
                            <*> lift (convert_type name_context ty_ast))
                    fields

convert_type :: IR.NameContext -> AST.Type -> MakeIRState TypeExpr
convert_type nc (AST.Type'Identifier id) = pure $ IR.TypeExpr'Identifier (Location.just_span id) (nc, Location.unlocate id)
convert_type nc (AST.Type'Tuple sp items) = mapM (convert_type nc) items >>= group_items
    where
        group_items [a, b] = pure $ IR.TypeExpr'Tuple a b
        group_items (a:b:more) = IR.TypeExpr'Tuple a <$> group_items (b:more)
        group_items [_] = tell_err (Tuple1 sp) >> pure (IR.TypeExpr'Poison sp)
        group_items [] = tell_err (Tuple0 sp) >> pure (IR.TypeExpr'Poison sp)

convert_expr :: IR.NameContext -> AST.Expr -> MakeIRState Expr
convert_expr nc (AST.Expr'Identifier iden) = pure $ IR.Expr'Identifier () (Location.just_span iden) (nc, Location.unlocate iden)
convert_expr _ (AST.Expr'Char sp c) = pure $ IR.Expr'Char () sp c
convert_expr _ (AST.Expr'String sp s) = pure $ IR.Expr'String () sp s
convert_expr _ (AST.Expr'Int sp i) = pure $ IR.Expr'Int () sp i
convert_expr _ (AST.Expr'Float sp f) = pure $ IR.Expr'Float () sp f
convert_expr _ (AST.Expr'Bool sp b) = pure $ IR.Expr'Bool () sp b

convert_expr parent_context (AST.Expr'Tuple sp items) =
    mapM (convert_expr parent_context) items >>= group_items
    where
        group_items [a, b] = pure $ IR.Expr'Tuple () sp a b
        group_items (a:b:more) = IR.Expr'Tuple () sp a <$> group_items (b:more) -- TODO: properly do span of b:more because this just takes the span of the whole thing
        group_items [_] = tell_err (Tuple1 sp) >> pure (IR.Expr'Poison () sp)
        group_items [] = tell_err (Tuple0 sp) >> pure (IR.Expr'Poison () sp)

convert_expr parent_context (AST.Expr'Lambda sp params body) = convert_lambda parent_context params body
    where
        convert_lambda parent_context (param:more) body =
            convert_pattern param >>= \ (bound_name_list, param) ->
            make_name_context [] bound_name_list (Just parent_context) >>= \ lambda_nc ->
            IR.Expr'Lambda () sp param <$> convert_lambda lambda_nc more body -- TODO: properly do spans of parts because this also just takes the whole span

        convert_lambda parent_context [] body = convert_expr parent_context body

convert_expr parent_context (AST.Expr'Let sp decls subexpr) =
    convert_decls (Just parent_context) decls >>= \ let_context ->
    IR.Expr'Let () sp <$> convert_expr let_context subexpr -- TODO: actually do sequentially because convert_decls does all at once
convert_expr parent_context (AST.Expr'LetRec sp decls subexpr) =
    convert_decls (Just parent_context) decls >>= \ let_context ->
    IR.Expr'LetRec () sp <$> convert_expr let_context subexpr

convert_expr parent_context (AST.Expr'BinaryOps sp first ops) = IR.Expr'BinaryOps () () sp <$> convert_expr parent_context first <*> mapM (\ (op, right) -> convert_expr parent_context right >>= \ right' -> pure ((parent_context, Location.unlocate op), right')) ops

convert_expr parent_context (AST.Expr'Call sp callee args) =
    convert_expr parent_context callee >>= \ callee ->
    foldlM (\ callee arg -> IR.Expr'Call () sp callee <$> convert_expr parent_context arg) callee args

convert_expr parent_context (AST.Expr'If sp if_sp cond t f) = IR.Expr'If () sp if_sp <$> convert_expr parent_context cond <*> convert_expr parent_context t <*> convert_expr parent_context f
convert_expr parent_context (AST.Expr'Case sp case_sp e arms) =
    convert_expr parent_context e >>= \ e ->
    mapM
        (\ (pat, choice) ->
            convert_pattern pat >>= \ (new_bound_names, pat) ->
            make_name_context [] new_bound_names (Just parent_context) >>= \ arm_nc ->
            convert_expr arm_nc choice >>= \ choice ->
            pure (pat, choice))
        arms
        >>= \ arms ->
    pure (IR.Expr'Case () sp case_sp e arms)

convert_expr nc (AST.Expr'TypeAnnotation sp ty e) = IR.Expr'TypeAnnotation () sp <$> convert_type nc ty <*> convert_expr nc e

convert_pattern :: AST.Pattern -> MakeIRState (BoundNameList, Pattern)
convert_pattern (AST.Pattern'Identifier iden) =
    make_iden1_with_err PathInPattern iden >>= \case
        Just l_name@(Location.Located name_sp name) ->
            new_bound_name name name_sp >>= \ bn ->
            pure ([(l_name, bn)], IR.Pattern'Identifier () name_sp bn)

        Nothing -> pure ([], IR.Pattern'Poison () (Location.just_span iden))
convert_pattern (AST.Pattern'Tuple sp subpats) =
    List.unzip <$> mapM convert_pattern subpats >>= \ (bound_names, subpats') ->
    go subpats' >>= \ subpats_grouped ->
    pure (concat bound_names, subpats_grouped)
    where
        go [a, b] = pure $ IR.Pattern'Tuple () sp a b
        go (a:b:more) = IR.Pattern'Tuple () sp a <$> go (b:more)
        go [_] = tell_err (Tuple1 sp) >> pure (IR.Pattern'Poison () sp)
        go [] = tell_err (Tuple0 sp) >> pure (IR.Pattern'Poison () sp)
convert_pattern (AST.Pattern'Named sp iden at_sp subpat) =
    convert_pattern subpat >>= \ (sub_bn, subpat') ->
    make_iden1_with_err PathInPattern iden >>= \case
        Just l_name@(Location.Located name_sp name) ->
            new_bound_name name name_sp >>= \ bn ->
            pure ((l_name, bn) : sub_bn, IR.Pattern'Named () sp at_sp (Location.Located name_sp bn) subpat')

        Nothing ->
            pure ([], IR.Pattern'Poison () sp)
