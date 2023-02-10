module UHF.ASTToIR
    ( Decl
    , Module
    , Binding
    , Expr
    , Pattern

    , DeclArena
    , BoundNameArena
    , BindingArena

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

data Error
    = Redefinition (Location.Located Text)
    | BindToPath (Location.Located [Location.Located Text])

instance Diagnostic.IsError Error where
    to_error (Redefinition (Location.Located sp name)) = Diagnostic.Error Codes.symbol_redefinition $
        Diagnostic.DiagnosticContents
            (Just sp)
            [Underlines.underlines [sp `Underlines.primary` [Underlines.error $ "redefinition of '" <> convert_str name <> "'"]]]

    to_error (BindToPath (Location.Located sp _)) = Diagnostic.Error Codes.binding_lhs_path $
        Diagnostic.DiagnosticContents
            (Just sp)
            [ Underlines.underlines
                [sp `Underlines.primary` [Underlines.error "path in pattern"]]
            ]

type Decl = IR.Decl
type Module = IR.Module
type Binding = IR.Binding (Location.Located [Location.Located Text])
type Expr = IR.Expr (Location.Located [Location.Located Text])
type Pattern = IR.Pattern (Location.Located [Location.Located Text])

type DeclArena = Arena.Arena Decl IR.DeclKey
type BindingArena = Arena.Arena Binding IR.BindingKey
type BoundNameArena = Arena.Arena IR.BoundName IR.BoundNameKey

type DeclMap = Map.Map Text IR.DeclKey
type BoundNameMap = Map.Map Text IR.BoundNameKey
type ChildMaps = (DeclMap, BoundNameMap)

type MakeIRState a = StateT (DeclArena, BindingArena, BoundNameArena) (Writer [Error]) a

put_decl :: Decl -> MakeIRState IR.DeclKey
put_decl d =
    state $ \ (decls, bindings, bound_names) ->
        let (key, decls') = Arena.put d decls
        in (key, (decls', bindings, bound_names))

put_binding :: Binding -> MakeIRState IR.BindingKey
put_binding v =
    state $ \ (decls, bindings, bound_names) ->
        let (key, bindings') = Arena.put v bindings
        in (key, (decls, bindings', bound_names))

new_bound_name :: Text -> MakeIRState IR.BoundNameKey
new_bound_name _ =
    state $ \ (decls, bindings, bound_names) ->
        let (key, bound_names') = Arena.put (IR.BoundName ()) bound_names
        in (key, (decls, bindings, bound_names'))

tell_err :: Error -> MakeIRState ()
tell_err = lift . tell . (:[])

convert :: [AST.Decl] -> Writer [Error] (DeclArena, BindingArena, BoundNameArena, IR.DeclKey)
convert decls =
    let make =
            convert_to_maps decls >>= \ (decl_map, bound_name_map) ->
            put_decl (IR.Decl'Module $ IR.Module decl_map bound_name_map)

    in runStateT make (Arena.new, Arena.new, Arena.new) >>= \ (mod, (decls, values, bound_names)) ->
    pure (decls, values, bound_names, mod)

convert_to_maps :: [AST.Decl] -> MakeIRState ChildMaps
convert_to_maps =
    foldl'
        (\ last_map cur_decl ->
            last_map >>= \ (last_decl_map, last_bound_name_map) ->
            case cur_decl of
                AST.Decl'Value target expr ->
                    convert_expr expr >>= \ expr' ->
                    convert_pattern last_bound_name_map target >>= \ (bound_name_map', target') ->
                    put_binding (IR.Binding target' expr') >>
                    pure (last_decl_map, bound_name_map')

                AST.Decl'Data name variants -> todo
                AST.Decl'TypeSyn name expansion -> todo
            )
        (pure (Map.empty, Map.empty))

convert_expr :: AST.Expr -> MakeIRState Expr
convert_expr (AST.Expr'Identifier iden) = pure $ IR.Expr'Identifier iden
convert_expr (AST.Expr'Char c) = pure $ IR.Expr'Char c
convert_expr (AST.Expr'String s) = pure $ IR.Expr'String s
convert_expr (AST.Expr'Int i) = pure $ IR.Expr'Int i
convert_expr (AST.Expr'Float f) = pure $ IR.Expr'Float f
convert_expr (AST.Expr'Bool b) = pure $ IR.Expr'Bool b

convert_expr (AST.Expr'Tuple items) = IR.Expr'Tuple <$> (mapM convert_expr items)

convert_expr (AST.Expr'Lambda params body) =
    mapAccumLM convert_pattern Map.empty params >>= \ (map, params) ->
    convert_expr body >>= \ body ->
    pure (IR.Expr'Lambda map params body)

convert_expr (AST.Expr'Let decls subexpr) =
    convert_to_maps decls >>= \ (d_map, bn_map) ->
    IR.Expr'Let d_map bn_map <$> (convert_expr subexpr)
convert_expr (AST.Expr'LetRec decls subexpr) =
    convert_to_maps decls >>= \ (d_map, bn_map) ->
    IR.Expr'Let d_map bn_map <$> (convert_expr subexpr)

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

add_iden_to_bound_name_map :: BoundNameMap -> Location.Located [Location.Located Text] -> MakeIRState (Maybe (BoundNameMap, IR.BoundNameKey))
add_iden_to_bound_name_map map (Location.Located _ [l_name1@(Location.Located _ name1)]) =
    case Map.lookup name1 map of
        Just _ -> tell_err (Redefinition l_name1) >> pure Nothing
        Nothing ->
            new_bound_name name1 >>= \ bound_name ->
            pure $ Just (Map.insert name1 bound_name map, bound_name)

add_iden_to_bound_name_map _ l_name = tell_err (BindToPath l_name) >> pure Nothing

convert_pattern :: BoundNameMap -> AST.Pattern -> MakeIRState (BoundNameMap, Pattern)
convert_pattern map (AST.Pattern'Identifier iden) =
    add_iden_to_bound_name_map map iden >>= \case
        Just (map', iden') -> pure (map', IR.Pattern'Identifier iden')
        Nothing -> todo
convert_pattern map (AST.Pattern'Tuple subpats) =
    mapAccumLM convert_pattern map subpats >>= \ (map', subpats') ->
    pure (map', IR.Pattern'Tuple subpats')
convert_pattern map (AST.Pattern'Named iden subpat) =
    add_iden_to_bound_name_map map iden >>= \case
        Just (map, iden) ->
            convert_pattern map subpat >>= \ (map, subpat) ->
            pure (map, IR.Pattern'Named iden subpat)
        Nothing -> todo

mapAccumLM :: Monad m => (acc -> a -> m (acc, b)) -> acc -> [a] -> m (acc, [b])
mapAccumLM f acc (x:xs) =
    f acc x >>= \ (acc, x') ->
    mapAccumLM f acc xs >>= \ (acc, xs') ->
    pure (acc, x' : xs')
mapAccumLM _ acc [] = pure (acc, [])
