module UHF.NameResolve
    ( UnresolvedBinding
    , ResolvedBinding
    , UnresolvedExpr
    , ResolvedExpr
    , UnresolvedPattern
    , ResolvedPattern

    , UnresolvedNominalTypeArena
    , ResolvedNominalTypeArena
    , UnresolvedBindingArena
    , ResolvedBindingArena

    , DeclArena

    , resolve
    ) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.Compiler as Compiler

import qualified UHF.IO.Located as Located
import UHF.IO.Located (Located (Located))
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Diagnostic.Codes

import qualified UHF.IR as IR

import qualified Data.Map as Map
import Data.Functor.Identity (Identity (Identity, runIdentity))

type UnresolvedTypeIdentifier = (IR.NameContext, [Located Text])
type UnresolvedExprIdentifier = (IR.NameContext, [Located Text])
type UnresolvedNominalType = IR.NominalType UnresolvedType
type UnresolvedType = IR.TypeExpr UnresolvedTypeIdentifier
type UnresolvedBinding = IR.Binding UnresolvedExprIdentifier UnresolvedType () ()
type UnresolvedExpr = IR.Expr UnresolvedExprIdentifier UnresolvedType () ()
type UnresolvedPattern = IR.Pattern UnresolvedExprIdentifier

type UnresolvedBindingArena = Arena.Arena UnresolvedBinding IR.BindingKey
type UnresolvedNominalTypeArena = Arena.Arena UnresolvedNominalType IR.NominalTypeKey

type ResolvedNominalType = IR.NominalType ResolvedType
type ResolvedType = IR.TypeExpr (Maybe IR.DeclKey)
type ResolvedBinding = IR.Binding (Located (Maybe IR.BoundValueKey)) ResolvedType () ()
type ResolvedExpr = IR.Expr (Located (Maybe IR.BoundValueKey)) ResolvedType () ()
type ResolvedPattern = IR.Pattern (Located (Maybe IR.BoundValueKey))

type ResolvedBindingArena = Arena.Arena ResolvedBinding IR.BindingKey
type ResolvedNominalTypeArena = Arena.Arena ResolvedNominalType IR.NominalTypeKey

type Decl = IR.Decl

type DeclArena = Arena.Arena Decl IR.DeclKey

data Error
    = CouldNotFind (Maybe (Located Text)) (Located Text)

instance Diagnostic.ToError Error where
    to_error (CouldNotFind prev (Located sp name)) =
        let message =
                "could not find name '" <> name <> "'"
                    <> case prev of
                        Just (Located _ prev_name) -> "in '" <> prev_name <> "'"
                        Nothing -> ""
        in Diagnostic.Error Diagnostic.Codes.undef_name (Just sp) message [] []

transform_identifiers :: Monad m => (t_iden -> m t_iden') -> (e_iden -> m e_iden') -> Arena.Arena (IR.NominalType (IR.TypeExpr t_iden)) IR.NominalTypeKey -> Arena.Arena (IR.Binding e_iden (IR.TypeExpr t_iden) typeinfo binaryopsallowed) IR.BindingKey -> m (Arena.Arena (IR.NominalType (IR.TypeExpr t_iden')) IR.NominalTypeKey, Arena.Arena (IR.Binding e_iden' (IR.TypeExpr t_iden') typeinfo binaryopsallowed) IR.BindingKey)
transform_identifiers transform_t_iden transform_e_iden nominal_types bindings = (,) <$> Arena.transformM transform_nominal_type nominal_types <*> Arena.transformM transform_binding bindings
    where
        transform_nominal_type (IR.NominalType'Data name variants) = IR.NominalType'Data name <$> mapM transform_variant variants
            where
                transform_variant (IR.DataVariant'Named name fields) = IR.DataVariant'Named name <$> mapM (\ (name, ty) -> (,) name <$> transform_type_expr ty) fields
                transform_variant (IR.DataVariant'Anon name fields) = IR.DataVariant'Anon name <$> mapM transform_type_expr fields
        transform_nominal_type (IR.NominalType'Synonym name expansion) = IR.NominalType'Synonym name <$> transform_type_expr expansion

        transform_type_expr (IR.TypeExpr'Identifier sp id) = IR.TypeExpr'Identifier sp <$> transform_t_iden id
        transform_type_expr (IR.TypeExpr'Tuple a b) = IR.TypeExpr'Tuple <$> transform_type_expr a <*> transform_type_expr b
        transform_type_expr (IR.TypeExpr'Poison sp) = pure $ IR.TypeExpr'Poison sp

        transform_binding (IR.Binding target eq_sp expr) = IR.Binding <$> transform_pat target <*> pure eq_sp <*> transform_expr expr

        transform_pat (IR.Pattern'Identifier typeinfo sp bnk) = pure $ IR.Pattern'Identifier typeinfo sp bnk
        transform_pat (IR.Pattern'Wildcard typeinfo sp) = pure $ IR.Pattern'Wildcard typeinfo sp
        transform_pat (IR.Pattern'Tuple typeinfo sp a b) = IR.Pattern'Tuple typeinfo sp <$> transform_pat a <*> transform_pat b
        transform_pat (IR.Pattern'Named typeinfo sp at_sp bnk subpat) = IR.Pattern'Named typeinfo sp at_sp bnk <$> transform_pat subpat
        transform_pat (IR.Pattern'Poison typeinfo sp) = pure $ IR.Pattern'Poison typeinfo sp

        transform_expr (IR.Expr'Identifier typeinfo sp i) = IR.Expr'Identifier typeinfo sp <$> transform_e_iden i
        transform_expr (IR.Expr'Char typeinfo sp c) = pure $ IR.Expr'Char typeinfo sp c
        transform_expr (IR.Expr'String typeinfo sp s) = pure $ IR.Expr'String typeinfo sp s
        transform_expr (IR.Expr'Int typeinfo sp i) = pure $ IR.Expr'Int typeinfo sp i
        transform_expr (IR.Expr'Float typeinfo sp f) = pure $ IR.Expr'Float typeinfo sp f
        transform_expr (IR.Expr'Bool typeinfo sp b) = pure $ IR.Expr'Bool typeinfo sp b

        transform_expr (IR.Expr'Tuple typeinfo sp a b) = IR.Expr'Tuple typeinfo sp <$> transform_expr a <*> transform_expr b

        transform_expr (IR.Expr'Lambda typeinfo sp param body) = IR.Expr'Lambda typeinfo sp <$> transform_pat param <*> transform_expr body

        transform_expr (IR.Expr'Let typeinfo sp body) = IR.Expr'Let typeinfo sp <$> transform_expr body
        transform_expr (IR.Expr'LetRec typeinfo sp body) = IR.Expr'LetRec typeinfo sp <$> transform_expr body

        transform_expr (IR.Expr'BinaryOps allowed typeinfo sp first ops) = IR.Expr'BinaryOps allowed typeinfo sp <$> transform_expr first <*> mapM (\ (iden, rhs) -> (,) <$> transform_e_iden iden <*> transform_expr rhs) ops

        transform_expr (IR.Expr'Call typeinfo sp callee arg) = IR.Expr'Call typeinfo sp <$> transform_expr callee <*> transform_expr arg

        transform_expr (IR.Expr'If typeinfo sp if_sp cond t f) = IR.Expr'If typeinfo sp if_sp <$> transform_expr cond <*> transform_expr t <*> transform_expr f
        transform_expr (IR.Expr'Case typeinfo sp case_sp e arms) = IR.Expr'Case typeinfo sp case_sp <$> transform_expr e <*> mapM (\ (pat, expr) -> (,) <$> transform_pat pat <*> transform_expr expr) arms

        transform_expr (IR.Expr'TypeAnnotation typeinfo sp ty e) = IR.Expr'TypeAnnotation typeinfo sp <$> transform_type_expr ty <*> transform_expr e

        transform_expr (IR.Expr'Poison typeinfo sp) = pure $ IR.Expr'Poison typeinfo sp

resolve :: (DeclArena, UnresolvedNominalTypeArena, UnresolvedBindingArena) -> Compiler.Compiler (DeclArena, ResolvedNominalTypeArena, ResolvedBindingArena)
resolve (decls, nominals, bindings) =
    let (res, errs) =
            runWriter (
                let (nominals', bindings') = runIdentity (transform_identifiers Identity split_expr_iden nominals bindings)
                in transform_identifiers (resolve_type_iden decls) (resolve_expr_iden decls) nominals' bindings' >>= \ (nominals', bindings') ->
                pure (decls, nominals', bindings')
            )
    in Compiler.errors errs >> pure res

split_expr_iden :: UnresolvedExprIdentifier -> Identity (IR.NameContext, Maybe [Located Text], Located Text)
split_expr_iden (_, []) = error "empty identifier"
split_expr_iden (nc, [x]) = pure (nc, Nothing, x)
split_expr_iden (nc, x) = pure (nc, Just $ init x, last x)

resolve_expr_iden :: DeclArena -> (IR.NameContext, Maybe [Located Text], Located Text) -> Writer [Error] (Located (Maybe IR.BoundValueKey))
resolve_expr_iden decls (nc, Just type_iden, last_segment) =
    let sp = Located.just_span (head type_iden) <> Located.just_span last_segment
    in resolve_type_iden decls (nc, type_iden) >>= \ resolved_type ->
    case get_value_child decls <$> resolved_type <*> pure last_segment of
        Just (Right v) -> pure $ Located sp (Just v)
        Just (Left e) -> tell [e] >> pure (Located sp Nothing)
        Nothing -> pure $ Located sp Nothing

resolve_expr_iden _ (nc, Nothing, last_segment@(Located last_segment_sp _)) =
    case resolve nc last_segment of
        Right v -> pure $ Located last_segment_sp (Just v)
        Left e -> tell [e] >> pure (Located last_segment_sp Nothing)
    where
        resolve (IR.NameContext _ bn_children parent) name =
            case Map.lookup (Located.unlocate name) bn_children of
                Just res -> Right res
                Nothing ->
                    case parent of
                        Just parent -> resolve parent name
                        Nothing -> Left $ CouldNotFind Nothing name

resolve_type_iden :: DeclArena -> UnresolvedTypeIdentifier -> Writer [Error] (Maybe IR.DeclKey)
resolve_type_iden _ (_, []) = error "empty identifier"
resolve_type_iden decls (nc, first:more) =
    case resolve_first nc first of
        Right first_resolved ->
            case foldlM (get_decl_child decls) first_resolved more of
                Right r -> pure $ Just r
                Left e -> tell [e] >> pure Nothing
        Left e -> tell [e] >> pure Nothing
    where
        resolve_first (IR.NameContext d_children _ parent) first =
            case Map.lookup (Located.unlocate first) d_children of
                Just decl -> Right decl
                Nothing ->
                    case parent of
                        Just parent -> resolve_first parent first
                        Nothing -> Left $ CouldNotFind Nothing first -- TODO: put previous in error

get_decl_child :: DeclArena -> IR.DeclKey -> Located Text -> Either Error IR.DeclKey
get_decl_child decls thing name =
    let res = case Arena.get decls thing of
            IR.Decl'Module (IR.NameContext d_children _ _) -> Map.lookup (Located.unlocate name) d_children
            IR.Decl'Type _ -> Nothing -- TODO: implement children of types through impl blocks, this will also need infinite recursion checking
    in case res of
        Just res -> Right res
        Nothing -> Left $ CouldNotFind Nothing name -- TODO: put previous

get_value_child :: DeclArena -> IR.DeclKey -> Located Text -> Either Error IR.BoundValueKey
get_value_child decls thing name =
    let res = case Arena.get decls thing of
            IR.Decl'Module (IR.NameContext _ v_children _) -> Map.lookup (Located.unlocate name) v_children
            IR.Decl'Type _ -> Nothing -- TODO: implement children of types through impl blocks
    in case res of
        Just res -> Right res
        Nothing -> Left $ CouldNotFind Nothing name -- TODO: put previous
