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
import qualified UHF.Diagnostic.Sections.Underlines as Underlines

import qualified Data.Map as Map

import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Control.Monad.Fix (mfix)

data Error
    = Redefinition (Location.Located Text)
    | PathInPattern (Location.Located [Location.Located Text]) -- TODO: make this less repetitive
    | PathInTypeName (Location.Located [Location.Located Text])
    | PathInVariantName (Location.Located [Location.Located Text])
    | PathInFieldName (Location.Located [Location.Located Text])

    | Tuple1 (Location.Span)
    | Tuple0 (Location.Span)

instance Diagnostic.IsError Error where
    to_error (Redefinition (Location.Located sp name)) = Diagnostic.Error Codes.symbol_redefinition $
        Diagnostic.DiagnosticContents
            (Just sp)
            [Underlines.underlines [sp `Underlines.primary` [Underlines.error $ "redefinition of '" <> convert_str name <> "'"]]]

    to_error (PathInPattern (Location.Located sp _)) = Diagnostic.Error Codes.binding_lhs_path $
        Diagnostic.DiagnosticContents
            (Just sp)
            [ Underlines.underlines
                [sp `Underlines.primary` [Underlines.error "path in pattern"]]
            ]

    to_error (PathInTypeName (Location.Located sp _)) = Diagnostic.Error Codes.path_in_type_name $
        Diagnostic.DiagnosticContents
            (Just sp)
            [ Underlines.underlines
                [sp `Underlines.primary` [Underlines.error "path in type name"]]
            ]

    to_error (PathInVariantName (Location.Located sp _)) = Diagnostic.Error Codes.path_in_variant_name $ -- TODO: remove codes?
        Diagnostic.DiagnosticContents
            (Just sp)
            [ Underlines.underlines
                [sp `Underlines.primary` [Underlines.error "path in 'data' variant name"]]
            ]

    to_error (PathInFieldName (Location.Located sp _)) = Diagnostic.Error Codes.path_in_field_name $
        Diagnostic.DiagnosticContents
            (Just sp)
            [ Underlines.underlines
                [sp `Underlines.primary` [Underlines.error "path in field name"]]
            ]

    to_error (Tuple1 sp) = Diagnostic.Error Codes.tuple1 $
        Diagnostic.DiagnosticContents
            (Just sp)
            [ Underlines.underlines
                [sp `Underlines.primary` [Underlines.error "tuple of 1 element"]]
            ]

    to_error (Tuple0 sp) = Diagnostic.Error Codes.tuple0 $
        Diagnostic.DiagnosticContents
            (Just sp)
            [ Underlines.underlines
                [sp `Underlines.primary` [Underlines.error "tuple of 0 element"]]
            ]

type Decl = IR.Decl
type Module = IR.Module
type Binding = IR.Binding (Location.Located [Location.Located Text])
type NominalType = IR.NominalType Type
type Type = IR.TypeExpr (Location.Located [Location.Located Text])
type Expr = IR.Expr (Location.Located [Location.Located Text])
type Pattern = IR.Pattern (Location.Located [Location.Located Text])

type DeclArena = Arena.Arena Decl IR.DeclKey
type BindingArena = Arena.Arena Binding IR.BindingKey
type BoundNameArena = Arena.Arena IR.BoundName IR.BoundNameKey
type NominalTypeArena = Arena.Arena NominalType IR.NominalTypeKey

type DeclMap = Map.Map Text IR.DeclKey
type BoundNameMap = Map.Map Text IR.BoundNameKey

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

new_bound_name :: Text -> MakeIRState IR.BoundNameKey
new_bound_name _ =
    state $ \ (decls, bindings, bound_names, nominals) ->
        let (key, bound_names') = Arena.put (IR.BoundName ()) bound_names
        in (key, (decls, bindings, bound_names', nominals))

new_nominal_type :: NominalType -> MakeIRState IR.NominalTypeKey
new_nominal_type nominal =
    state $ \ (decls, bindings, bound_names, nominals) ->
        let (key, nominals') = Arena.put nominal nominals
        in (key, (decls, bindings, bound_names, nominals'))

tell_err :: Error -> MakeIRState ()
tell_err = lift . tell . (:[])

make_iden1 :: Location.Located [Location.Located Text] -> Maybe (Location.Located Text)
make_iden1 (Location.Located _ [iden1]) = Just iden1
make_iden1 _ = Nothing

add_iden_to_decl_map :: DeclMap -> Location.Located Text -> IR.DeclKey -> MakeIRState (Maybe DeclMap)
add_iden_to_decl_map map l_name@(Location.Located _ name) decl_key =
    case Map.lookup name map of
        Just _ -> tell_err (Redefinition l_name) >> pure Nothing
        Nothing -> pure $ Just (Map.insert name decl_key map)

add_iden_to_bound_name_map :: BoundNameMap -> Location.Located [Location.Located Text] -> MakeIRState (Maybe (BoundNameMap, IR.BoundNameKey))
add_iden_to_bound_name_map map l_name =
    case make_iden1 l_name of
        Just l_name@(Location.Located _ name) ->
            case Map.lookup name map of
                Just _ -> tell_err (Redefinition l_name) >> pure Nothing
                Nothing ->
                    new_bound_name name >>= \ bound_name ->
                    pure $ Just (Map.insert name bound_name map, bound_name)

        _ -> tell_err (PathInPattern l_name) >> pure Nothing

add_decl_to_name_context :: IR.NameContext -> Location.Located Text -> IR.DeclKey -> MakeIRState (Maybe IR.NameContext)
add_decl_to_name_context (IR.NameContext decls bns parent) l_name decl_key =
    add_iden_to_decl_map decls l_name decl_key >>= \case
        Just decls -> pure $ Just $ IR.NameContext decls bns parent
        Nothing -> pure Nothing

add_bound_name_to_name_context :: IR.NameContext -> Location.Located [Location.Located Text] -> MakeIRState (Maybe (IR.NameContext, IR.BoundNameKey))
add_bound_name_to_name_context (IR.NameContext decls bns parent) l_name =
    add_iden_to_bound_name_map bns l_name >>= \case
        Just (bns, bnk) -> pure $ Just (IR.NameContext decls bns parent, bnk)
        Nothing -> pure Nothing

convert :: [AST.Decl] -> Writer [Error] (DeclArena, NominalTypeArena, BindingArena, BoundNameArena, IR.DeclKey)
convert decls =
    let make = IR.Decl'Module <$> (IR.Module <$> convert_decls Nothing decls) >>= new_decl
    in runStateT make (Arena.new, Arena.new, Arena.new, Arena.new) >>= \ (mod, (decls, bindings, bound_names, nominals)) ->
    pure (decls, nominals, bindings, bound_names, mod)

convert_decls :: Maybe IR.NameContext -> [AST.Decl] -> MakeIRState IR.NameContext
convert_decls parent_context decls =
    mfix (\ resulting_name_context ->
        foldlM
            (\ name_context cur_decl ->
                case cur_decl of
                    AST.Decl'Value target expr ->
                        convert_expr resulting_name_context expr >>= \ expr' ->
                        convert_pattern name_context target >>= \ (name_context, target') ->
                        new_binding (IR.Binding target' expr') >>
                        pure name_context

                    AST.Decl'Data name variants ->
                        runMaybeT (
                            IR.NominalType'Data <$> mapM convert_variant variants >>= \ datatype ->

                            lift (new_nominal_type datatype) >>= \ nominal_type_key ->
                            lift (new_decl (IR.Decl'Type nominal_type_key)) >>= \ decl_key ->

                            iden1_for_datatype_name name >>= \ name1 ->
                            MaybeT (add_decl_to_name_context name_context name1 decl_key)
                        ) >>= \ name_context' ->
                        pure (fromMaybe name_context name_context')

                    AST.Decl'TypeSyn name expansion ->
                        convert_type expansion >>= \ expansion' ->
                        new_nominal_type (IR.NominalType'Synonym expansion') >>= \ nominal_type_key ->
                        new_decl (IR.Decl'Type nominal_type_key) >>= \ decl_key ->

                        runMaybeT (
                            iden1_for_datatype_name name >>= \ name1 ->
                            MaybeT (add_decl_to_name_context name_context name1 decl_key)
                        ) >>= \ name_context' ->
                        pure (fromMaybe name_context name_context')
            )
            (IR.NameContext  Map.empty Map.empty parent_context)
            decls
        )
    where
        iden1_for_variant_name iden = MaybeT $ case make_iden1 iden of
            Just (Location.Located _ n) -> pure (Just n)
            Nothing -> tell_err (PathInVariantName iden) >> pure Nothing

        iden1_for_datatype_name iden = MaybeT $ case make_iden1 iden of
            Just n -> pure (Just n)
            Nothing -> tell_err (PathInTypeName iden) >> pure Nothing

        iden1_for_field_name iden = MaybeT $ case make_iden1 iden of
            Just (Location.Located _ n) -> pure (Just n)
            Nothing -> tell_err (PathInFieldName iden) >> pure Nothing

        convert_variant (AST.DataVariant'Anon name fields) = IR.DataVariant'Anon <$> iden1_for_variant_name name <*> (lift $ mapM convert_type fields)
        convert_variant (AST.DataVariant'Named name fields) = IR.DataVariant'Named <$> iden1_for_variant_name name <*> mapM (\ (field_name, ty_ast) -> (,) <$> iden1_for_field_name field_name <*> lift (convert_type ty_ast)) fields

convert_type :: AST.Type -> MakeIRState Type
convert_type (AST.Type'Identifier id) = pure $ IR.TypeExpr'Identifier id
convert_type (AST.Type'Tuple items) = IR.TypeExpr'Tuple <$> mapM convert_type items

convert_expr :: IR.NameContext -> AST.Expr -> MakeIRState Expr
convert_expr _ (AST.Expr'Identifier iden) = pure $ IR.Expr'Identifier iden
convert_expr _ (AST.Expr'Char c) = pure $ IR.Expr'Char c
convert_expr _ (AST.Expr'String s) = pure $ IR.Expr'String s
convert_expr _ (AST.Expr'Int i) = pure $ IR.Expr'Int i
convert_expr _ (AST.Expr'Float f) = pure $ IR.Expr'Float f
convert_expr _ (AST.Expr'Bool b) = pure $ IR.Expr'Bool b

convert_expr parent_context (AST.Expr'Tuple items) =
    mapM (convert_expr parent_context) items >>= group_items
    where
        group_items (a:b:[]) = pure $ IR.Expr'Tuple a b
        group_items (a:b:more) = IR.Expr'Tuple a <$> (group_items $ b:more)
        group_items [_] = tell_err (Tuple1 todo) >> pure IR.Expr'Poison
        group_items [] = tell_err (Tuple0 todo) >> pure IR.Expr'Poison

convert_expr parent_context (AST.Expr'Lambda params body) = convert_lambda parent_context params body
    where
        convert_lambda parent_context (param:more) body =
            convert_pattern (IR.NameContext Map.empty Map.empty (Just parent_context)) param >>= \ (cur_nc, param) -> -- TODO: parent context
            (IR.Expr'Lambda cur_nc param <$> convert_lambda cur_nc more body)

        convert_lambda parent_context [] body = convert_expr parent_context body

convert_expr parent_context (AST.Expr'Let decls subexpr) =
    convert_decls (Just parent_context) decls >>= \ let_context ->
    IR.Expr'Let let_context <$> (convert_expr let_context subexpr) -- TODO: actually do sequentially because convert_decls does all at once
convert_expr parent_context (AST.Expr'LetRec decls subexpr) =
    convert_decls (Just parent_context) decls >>= \ let_context ->
    IR.Expr'LetRec let_context <$> (convert_expr let_context subexpr)

convert_expr parent_context (AST.Expr'BinaryOps first ops) = IR.Expr'BinaryOps <$> convert_expr parent_context first <*> mapM (\ (op, right) -> convert_expr parent_context right >>= \ right' -> pure (op, right')) ops

convert_expr parent_context (AST.Expr'Call callee args) = IR.Expr'Call <$> convert_expr parent_context callee <*> mapM (convert_expr parent_context) args

convert_expr parent_context (AST.Expr'If cond t f) = IR.Expr'If <$> convert_expr parent_context cond <*> convert_expr parent_context t <*> convert_expr parent_context f
convert_expr parent_context (AST.Expr'Case e arms) =
    convert_expr parent_context e >>= \ e ->
    mapM
        (\ (pat, choice) ->
            convert_pattern (IR.NameContext Map.empty Map.empty (Just parent_context)) pat >>= \ (nc, pat) -> -- TODO: parent context
            convert_expr nc choice >>= \ choice ->
            pure (nc, pat, choice))
        arms
        >>= \ arms ->
    pure (IR.Expr'Case e arms)

convert_expr _ (AST.Expr'TypeAnnotation _ _) = todo -- IR.Expr'TypeAnnotation ty e

convert_pattern :: IR.NameContext -> AST.Pattern -> MakeIRState (IR.NameContext, Pattern)
convert_pattern nc (AST.Pattern'Identifier iden) =
    add_bound_name_to_name_context nc iden >>= \case
        Just (nc', iden') -> pure (nc', IR.Pattern'Identifier iden')
        Nothing -> pure (nc, IR.Pattern'Poison)
convert_pattern nc (AST.Pattern'Tuple subpats) =
    mapAccumLM convert_pattern nc subpats >>= \ (nc', subpats') ->
    go subpats' >>= \ subpats_grouped ->
    pure (nc', subpats_grouped)
    where
        go (a:b:[]) = pure $ IR.Pattern'Tuple a b
        go (a:b:more) = IR.Pattern'Tuple a <$> (go $ b:more)
        go [_] = tell_err (Tuple1 todo) >> pure IR.Pattern'Poison
        go [] = tell_err (Tuple0 todo) >> pure IR.Pattern'Poison
convert_pattern nc (AST.Pattern'Named iden subpat) =
    add_bound_name_to_name_context nc iden >>= \case
        Just (nc, iden) ->
            convert_pattern nc subpat >>= \ (nc, subpat) ->
            pure (nc, IR.Pattern'Named iden subpat)
        Nothing -> pure (nc, IR.Pattern'Poison)

mapAccumLM :: Monad m => (acc -> a -> m (acc, b)) -> acc -> [a] -> m (acc, [b])
mapAccumLM f acc (x:xs) =
    f acc x >>= \ (acc, x') ->
    mapAccumLM f acc xs >>= \ (acc, xs') ->
    pure (acc, x' : xs')
mapAccumLM _ acc [] = pure (acc, [])
