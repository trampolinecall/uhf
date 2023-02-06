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
import qualified UHF.IR.Decl as Decl
import qualified UHF.IR.Value as Value
import qualified UHF.IR.Expr as Expr

import qualified UHF.IO.Location as Location

import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Codes
import qualified UHF.Diagnostic.Sections.Underlines as Underlines

import qualified Data.Map as Map

data Error = Redefinition (Location.Located Text)

instance Diagnostic.IsError Error where
    to_error (Redefinition (Location.Located sp name)) =
        Diagnostic.Error Codes.symbol_redefinition (Just sp)
            [Underlines.underlines [sp `Underlines.primary` [Underlines.error $ "redefinition of '" <> convert_str name <> "'"]]]

type Decl = Decl.Decl
type Module = Decl.Module
type Value = Value.Value (Location.Located [Location.Located Text])
type Expr = Expr.Expr (Location.Located [Location.Located Text])

type DeclArena = Arena.Arena Decl Decl.Key
type ValueArena = Arena.Arena Value Value.Key

type DeclMap = Map.Map Text Decl.Key
type ValueMap = Map.Map Text Value.Key
type ChildMaps = (DeclMap, ValueMap)

type MakeIRState a = StateT (DeclArena, ValueArena) (Writer [Error]) a

put_decl :: Decl -> MakeIRState Decl.Key
put_decl d =
    state $ \ (decls, values) ->
        let (key, decls') = Arena.put d decls
        in (key, (decls', values))

put_value :: Value -> MakeIRState Value.Key
put_value v =
    state $ \ (decls, values) ->
        let (key, values') = Arena.put v values
        in (key, (decls, values'))

tell_err :: Error -> MakeIRState ()
tell_err = lift . tell . (:[])

convert :: [AST.Decl] -> Writer [Error] (DeclArena, ValueArena, Decl.Key)
convert decls =
    let make =
            convert_to_maps decls >>= \ (decl_map, value_map) ->
            put_decl (Decl.Decl'Module $ Decl.Module decl_map value_map)

    in runStateT make (Arena.new, Arena.new) >>= \ (mod, (decls, values)) ->
    pure (decls, values, mod)

convert_to_maps :: [AST.Decl] -> MakeIRState ChildMaps
convert_to_maps decls =
    foldl'
        (\ last_map cur_decl ->
            last_map >>= \ (last_decl_map, last_value_map) ->
            case cur_decl of
                AST.Decl'Binding l_name@(Location.Located _ name) expr ->
                    case Map.lookup name last_value_map of
                        Just _ -> tell_err (Redefinition l_name) >> pure (last_decl_map, last_value_map)
                        _ ->
                            convert_expr_to_value expr >>= \ new_value ->
                            pure (last_decl_map, Map.insert name new_value last_value_map)
            )
        (pure (Map.empty, Map.empty))
        decls

convert_expr_to_value :: AST.Expr -> MakeIRState Value.Key
convert_expr_to_value = put_value . Value.Value . convert_expr

convert_expr :: AST.Expr -> Expr
convert_expr (AST.Expr'Identifier iden) = Expr.Identifier iden
convert_expr (AST.Expr'CharLit c) = Expr.CharLit c
convert_expr (AST.Expr'StringLit s) = Expr.StringLit s
convert_expr (AST.Expr'IntLit i) = Expr.IntLit i
convert_expr (AST.Expr'FloatLit f) = Expr.FloatLit f
convert_expr (AST.Expr'BoolLit b) = Expr.BoolLit b
