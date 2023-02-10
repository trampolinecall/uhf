module UHF.ASTToIR
    ( Decl
    , Module
    , Value
    , Expr
    , DeclArena
    , ValueArena

    , DeclMap
    , ValueMap
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
                [sp `Underlines.primary` [Underlines.error "path in left-hand side of binding"]]
            ]

type Decl = IR.Decl
type Module = IR.Module
type Value = IR.Value (Location.Located [Location.Located Text])
type Expr = IR.Expr (Location.Located [Location.Located Text])

type DeclArena = Arena.Arena Decl IR.DeclKey
type ValueArena = Arena.Arena Value IR.ValueKey

type DeclMap = Map.Map Text IR.DeclKey
type ValueMap = Map.Map Text IR.ValueKey
type ChildMaps = (DeclMap, ValueMap)

type MakeIRState a = StateT (DeclArena, ValueArena) (Writer [Error]) a

put_decl :: Decl -> MakeIRState IR.DeclKey
put_decl d =
    state $ \ (decls, values) ->
        let (key, decls') = Arena.put d decls
        in (key, (decls', values))

put_value :: Value -> MakeIRState IR.ValueKey
put_value v =
    state $ \ (decls, values) ->
        let (key, values') = Arena.put v values
        in (key, (decls, values'))

tell_err :: Error -> MakeIRState ()
tell_err = lift . tell . (:[])

convert :: [AST.Decl] -> Writer [Error] (DeclArena, ValueArena, IR.DeclKey)
convert decls =
    let make =
            convert_to_maps decls >>= \ (decl_map, value_map) ->
            put_decl (IR.Decl'Module $ IR.Module decl_map value_map)

    in runStateT make (Arena.new, Arena.new) >>= \ (mod, (decls, values)) ->
    pure (decls, values, mod)

convert_to_maps :: [AST.Decl] -> MakeIRState ChildMaps
convert_to_maps =
    foldl'
        (\ last_map cur_decl ->
            last_map >>= \ (last_decl_map, last_value_map) ->
            todo
            {- TODO
            case cur_decl of
                AST.Decl'Value l_name@(Location.Located _ name) expr ->
                    case name of
                        [l_name1@(Location.Located _ name1)] ->
                            case Map.lookup name1 last_value_map of
                                Just _ -> tell_err (Redefinition l_name1) >> pure (last_decl_map, last_value_map)
                                _ ->
                                    convert_expr_to_value expr >>= \ new_value ->
                                    pure (last_decl_map, Map.insert name1 new_value last_value_map)

                        _ ->
                            tell_err (BindToPath l_name) >> pure (last_decl_map, last_value_map)
            -}
            )
        (pure (Map.empty, Map.empty))

convert_expr_to_value :: AST.Expr -> MakeIRState IR.ValueKey
convert_expr_to_value = put_value . IR.Value . convert_expr

convert_expr :: AST.Expr -> Expr
convert_expr (AST.Expr'Identifier iden) = IR.Expr'Identifier iden
convert_expr (AST.Expr'Char c) = IR.Expr'Char c
convert_expr (AST.Expr'String s) = IR.Expr'String s
convert_expr (AST.Expr'Int i) = IR.Expr'Int i
convert_expr (AST.Expr'Float f) = IR.Expr'Float f
convert_expr (AST.Expr'Bool b) = IR.Expr'Bool b
