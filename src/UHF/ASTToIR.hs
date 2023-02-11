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
    , ChildMaps

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
type ChildMaps = (DeclMap, BoundNameMap)

type MakeIRState = StateT (DeclArena, BindingArena, BoundNameArena, NominalTypeArena) (Writer [Error])

put_decl :: Decl -> MakeIRState IR.DeclKey
put_decl d =
    state $ \ (decls, bindings, bound_names, nominals) ->
        let (key, decls') = Arena.put d decls
        in (key, (decls', bindings, bound_names, nominals))

put_binding :: Binding -> MakeIRState IR.BindingKey
put_binding v =
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

convert :: [AST.Decl] -> Writer [Error] (DeclArena, NominalTypeArena, BindingArena, BoundNameArena, IR.DeclKey)
convert decls =
    let make =
            convert_to_maps decls >>= \ (decl_map, bound_name_map) ->
            put_decl (IR.Decl'Module $ IR.Module decl_map bound_name_map)

    in runStateT make (Arena.new, Arena.new, Arena.new, Arena.new) >>= \ (mod, (decls, values, bound_names, nominals)) ->
    pure (decls, nominals, values, bound_names, mod)

convert_to_maps :: [AST.Decl] -> MakeIRState ChildMaps
convert_to_maps =
    foldl'
        (\ last_map cur_decl ->
            last_map >>= \ (decl_map, bound_name_map) ->
            case cur_decl of
                AST.Decl'Value target expr ->
                    convert_expr expr >>= \ expr' ->
                    convert_pattern bound_name_map target >>= \ (bound_name_map, target') ->
                    put_binding (IR.Binding target' expr') >>
                    pure (decl_map, bound_name_map)

                AST.Decl'Data name variants ->
                    runMaybeT (
                        IR.NominalType'Data <$> mapM convert_variant variants >>= \ datatype ->

                        lift (new_nominal_type datatype) >>= \ nominal_type_key ->
                        lift (put_decl (IR.Decl'Type nominal_type_key)) >>= \ decl_key ->

                        iden1_for_datatype_name name >>= \ name1 ->
                        MaybeT (add_iden_to_decl_map decl_map name1 decl_key) >>= \ decl_map' ->
                        pure (decl_map')
                    ) >>= \ decl_map' ->
                    pure (fromMaybe decl_map decl_map', bound_name_map)

                AST.Decl'TypeSyn name expansion ->
                    convert_type expansion >>= \ expansion' ->
                    new_nominal_type (IR.NominalType'Synonym expansion') >>= \ nominal_type_key ->
                    put_decl (IR.Decl'Type nominal_type_key) >>= \ decl_key ->

                    runMaybeT (
                        iden1_for_datatype_name name >>= \ name1 ->
                        MaybeT (add_iden_to_decl_map decl_map name1 decl_key) >>= \ decl_map' ->
                        pure (decl_map')
                    ) >>= \ decl_map' ->
                    pure (fromMaybe decl_map decl_map', bound_name_map)
            )
        (pure (Map.empty, Map.empty))
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

convert_expr :: AST.Expr -> MakeIRState Expr
convert_expr (AST.Expr'Identifier iden) = pure $ IR.Expr'Identifier iden
convert_expr (AST.Expr'Char c) = pure $ IR.Expr'Char c
convert_expr (AST.Expr'String s) = pure $ IR.Expr'String s
convert_expr (AST.Expr'Int i) = pure $ IR.Expr'Int i
convert_expr (AST.Expr'Float f) = pure $ IR.Expr'Float f
convert_expr (AST.Expr'Bool b) = pure $ IR.Expr'Bool b

convert_expr (AST.Expr'Tuple items) =
    mapM convert_expr items >>= group_items
    where
        group_items (a:b:[]) = pure $ IR.Expr'Tuple a b
        group_items (a:b:more) = IR.Expr'Tuple a <$> (group_items $ b:more)
        group_items [_] = tell_err (Tuple1 todo)>> pure IR.Expr'Poison
        group_items [] = tell_err (Tuple0 todo)>> pure IR.Expr'Poison

convert_expr (AST.Expr'Lambda params body) = convert_lambda params body
    where
        convert_lambda (param:more) body =
            convert_pattern Map.empty param >>= \ (map, param) ->
            (IR.Expr'Lambda map param <$> (convert_lambda more body))

        convert_lambda [] body = convert_expr body

convert_expr (AST.Expr'Let decls subexpr) = convert_to_maps decls >>= \ (d_map, bn_map) -> IR.Expr'Let d_map bn_map <$> (convert_expr subexpr) -- TODO: actually do sequentially because convert_to_maps does all at once
convert_expr (AST.Expr'LetRec decls subexpr) = convert_to_maps decls >>= \ (d_map, bn_map) -> IR.Expr'LetRec d_map bn_map <$> (convert_expr subexpr)

convert_expr (AST.Expr'BinaryOps first ops) = IR.Expr'BinaryOps <$> convert_expr first <*> mapM (\ (op, right) -> convert_expr right >>= \ right' -> pure (op, right')) ops

convert_expr (AST.Expr'Call callee args) = IR.Expr'Call <$> convert_expr callee <*> mapM convert_expr args

convert_expr (AST.Expr'If cond t f) = IR.Expr'If <$> convert_expr cond <*> convert_expr t <*> convert_expr f
convert_expr (AST.Expr'Case e arms) =
    convert_expr e >>= \ e ->
    mapM
        (\ (pat, choice) ->
            convert_pattern Map.empty pat >>= \ (bn_map, pat) ->
            convert_expr choice >>= \ choice ->
            pure (bn_map, pat, choice))
        arms
        >>= \ arms ->
    pure (IR.Expr'Case e arms)

convert_expr (AST.Expr'TypeAnnotation _ _) = todo -- IR.Expr'TypeAnnotation ty e

convert_pattern :: BoundNameMap -> AST.Pattern -> MakeIRState (BoundNameMap, Pattern)
convert_pattern map (AST.Pattern'Identifier iden) =
    add_iden_to_bound_name_map map iden >>= \case
        Just (map', iden') -> pure (map', IR.Pattern'Identifier iden')
        Nothing -> pure (map, IR.Pattern'Poison)
convert_pattern map (AST.Pattern'Tuple subpats) =
    mapAccumLM convert_pattern map subpats >>= \ (map', subpats') ->
    go subpats' >>= \ subpats_grouped ->
    pure (map', subpats_grouped)
    where
        go (a:b:[]) = pure $ IR.Pattern'Tuple a b
        go (a:b:more) = IR.Pattern'Tuple a <$> (go $ b:more)
        go [_] = tell_err (Tuple1 todo) >> pure IR.Pattern'Poison
        go [] = tell_err (Tuple0 todo) >> pure IR.Pattern'Poison
convert_pattern map (AST.Pattern'Named iden subpat) =
    add_iden_to_bound_name_map map iden >>= \case
        Just (map, iden) ->
            convert_pattern map subpat >>= \ (map, subpat) ->
            pure (map, IR.Pattern'Named iden subpat)
        Nothing -> pure (map, IR.Pattern'Poison)

mapAccumLM :: Monad m => (acc -> a -> m (acc, b)) -> acc -> [a] -> m (acc, [b])
mapAccumLM f acc (x:xs) =
    f acc x >>= \ (acc, x') ->
    mapAccumLM f acc xs >>= \ (acc, xs') ->
    pure (acc, x' : xs')
mapAccumLM _ acc [] = pure (acc, [])
