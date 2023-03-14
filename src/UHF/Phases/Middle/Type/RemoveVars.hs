module UHF.Phases.Middle.Type.RemoveVars (remove) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.HIR as HIR
import qualified UHF.Data.IR.Type as Type

import qualified UHF.Compiler as Compiler

import UHF.Phases.Middle.Type.Var
import UHF.Phases.Middle.Type.Aliases
import UHF.Phases.Middle.Type.Error

import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Control.Monad.Fix (mfix)

remove :: TypeVarArena -> TypedWithVarsDeclArena -> TypedWithVarsADTArena -> TypedWithVarsTypeSynonymArena -> TypedWithVarsBoundValueArena -> Compiler.WithDiagnostics Error Void (TypedDeclArena, TypedADTArena, TypedTypeSynonymArena, TypedBoundValueArena)
remove vars decls adts type_synonyms bvs =
    convert_vars vars >>= \ vars ->
    pure (Arena.transform (decl vars) decls, Arena.transform (adt vars) adts, Arena.transform (type_synonym vars) type_synonyms, Arena.transform (bound_value vars) bvs)

convert_vars :: TypeVarArena -> Compiler.WithDiagnostics Error Void (Arena.Arena (Maybe Type) TypeVarKey)
convert_vars vars =
    -- infinite recursion is not possible because occurs check prevents loops in substitution
    let (res, errs) = (runWriter $ mfix (\ vars_converted -> Arena.transformM (runMaybeT . convert_var vars_converted) vars))
    in Compiler.tell_errors errs >> pure res
    where
        -- this usage of mfix does not play nicely with the IO in the Compiler monad so this must be done in the Writer monad
        r _ Type.Type'Int = pure Type.Type'Int
        r _ Type.Type'Float = pure Type.Type'Float
        r _ Type.Type'Char = pure Type.Type'Char
        r _ Type.Type'String = pure Type.Type'String
        r _ Type.Type'Bool = pure Type.Type'Bool
        r _ (Type.Type'ADT a) = pure $ Type.Type'ADT a
        r _ (Type.Type'Synonym s) = pure $ Type.Type'Synonym s
        r vars_converted (Type.Type'Function arg res) = Type.Type'Function <$> r vars_converted arg <*> r vars_converted res
        r vars_converted (Type.Type'Tuple a b) = Type.Type'Tuple <$> r vars_converted a <*> r vars_converted b
        r vars_converted (Type.Type'Variable v) = MaybeT $ pure $ Arena.get vars_converted v

        convert_var vars_converted (TypeVar _ (Substituted s)) = r vars_converted s
        convert_var _ (TypeVar for_what Fresh) = lift (tell [AmbiguousType for_what]) >> MaybeT (pure Nothing)

decl :: Arena.Arena (Maybe Type) TypeVarKey -> TypedWithVarsDecl -> TypedDecl
decl vars (HIR.Decl'Module id nc bindings adts type_synonyms) = HIR.Decl'Module id nc (map (binding vars) bindings) adts type_synonyms
decl _ (HIR.Decl'Type ty) = HIR.Decl'Type ty

bound_value :: Arena.Arena (Maybe Type) TypeVarKey -> TypedWithVarsBoundValue -> TypedBoundValue
bound_value vars (HIR.BoundValue id ty sp) = HIR.BoundValue id (type_ vars ty) sp

adt :: Arena.Arena (Maybe Type) TypeVarKey -> TypedWithVarsADT -> TypedADT
adt vars (Type.ADT name variants) = Type.ADT name (map variant variants)
    where
        variant (Type.ADTVariant'Named name fields) = Type.ADTVariant'Named name (map (\ (name, ty) -> (name, type_ vars ty)) fields)
        variant (Type.ADTVariant'Anon name fields) = Type.ADTVariant'Anon name (map (type_ vars) fields)

type_synonym :: Arena.Arena (Maybe Type) TypeVarKey -> TypedWithVarsTypeSynonym -> TypedTypeSynonym
type_synonym vars (Type.TypeSynonym name expansion) = Type.TypeSynonym name (type_ vars expansion)

binding :: Arena.Arena (Maybe Type) TypeVarKey -> TypedWithVarsBinding -> TypedBinding
binding vars (HIR.Binding p eq_sp e) = HIR.Binding (pattern vars p) eq_sp (expr vars e)

pattern :: Arena.Arena (Maybe Type) TypeVarKey -> TypedWithVarsPattern -> TypedPattern
pattern vars (HIR.Pattern'Identifier ty sp bn) = HIR.Pattern'Identifier (type_ vars ty) sp bn
pattern vars (HIR.Pattern'Wildcard ty sp) = HIR.Pattern'Wildcard (type_ vars ty) sp
pattern vars (HIR.Pattern'Tuple ty sp l r) = HIR.Pattern'Tuple (type_ vars ty) sp (pattern vars l) (pattern vars r)
pattern vars (HIR.Pattern'Named ty sp at_sp bnk subpat) = HIR.Pattern'Named (type_ vars ty) sp at_sp bnk (pattern vars subpat)
pattern vars (HIR.Pattern'Poison ty sp) = HIR.Pattern'Poison (type_ vars ty) sp

expr :: Arena.Arena (Maybe Type) TypeVarKey -> TypedWithVarsExpr -> TypedExpr
expr vars (HIR.Expr'Identifier ty sp bn) = HIR.Expr'Identifier (type_ vars ty) sp bn
expr vars (HIR.Expr'Char ty sp c) = HIR.Expr'Char (type_ vars ty) sp c
expr vars (HIR.Expr'String ty sp t) = HIR.Expr'String (type_ vars ty) sp t
expr vars (HIR.Expr'Int ty sp i) = HIR.Expr'Int (type_ vars ty) sp i
expr vars (HIR.Expr'Float ty sp r) = HIR.Expr'Float (type_ vars ty) sp r
expr vars (HIR.Expr'Bool ty sp b) = HIR.Expr'Bool (type_ vars ty) sp b
expr vars (HIR.Expr'Tuple ty sp l r) = HIR.Expr'Tuple (type_ vars ty) sp (expr vars l) (expr vars r)
expr vars (HIR.Expr'Lambda ty sp param body) = HIR.Expr'Lambda (type_ vars ty) sp (pattern vars param) (expr vars body)
expr vars (HIR.Expr'Let ty sp bindings result) = HIR.Expr'Let (type_ vars ty) sp (map (binding vars) bindings) (expr vars result)
expr _ (HIR.Expr'BinaryOps void _ _ _ _) = absurd void
expr vars (HIR.Expr'Call ty sp callee arg) = HIR.Expr'Call (type_ vars ty) sp (expr vars callee) (expr vars arg)
expr vars (HIR.Expr'If ty sp if_sp cond true false) = HIR.Expr'If (type_ vars ty) sp if_sp (expr vars cond) (expr vars true) (expr vars false)
expr vars (HIR.Expr'Case ty sp case_sp testing arms) = HIR.Expr'Case (type_ vars ty) sp case_sp (expr vars testing) (map (\ (p, e) -> (pattern vars p, expr vars e)) arms)
expr vars (HIR.Expr'Poison ty sp) = HIR.Expr'Poison (type_ vars ty) sp
expr vars (HIR.Expr'TypeAnnotation ty sp annotation e) = HIR.Expr'TypeAnnotation (type_ vars ty) sp (type_ vars annotation) (expr vars e)

type_ :: Arena.Arena (Maybe Type) TypeVarKey -> TypeWithVars -> Maybe Type
type_ vars = r
    where
        r Type.Type'Int = pure Type.Type'Int
        r Type.Type'Float = pure Type.Type'Float
        r Type.Type'Char = pure Type.Type'Char
        r Type.Type'String = pure Type.Type'String
        r Type.Type'Bool = pure Type.Type'Bool
        r (Type.Type'ADT a) = pure $ Type.Type'ADT a
        r (Type.Type'Synonym s) = pure $ Type.Type'Synonym s
        r (Type.Type'Function arg res) = Type.Type'Function <$> r arg <*> r res
        r (Type.Type'Tuple a b) = Type.Type'Tuple <$> r a <*> r b
        r (Type.Type'Variable v) = Arena.get vars v
