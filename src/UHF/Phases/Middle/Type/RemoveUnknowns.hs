module UHF.Phases.Middle.Type.RemoveUnknowns (remove) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type

import qualified UHF.Compiler as Compiler

import UHF.Phases.Middle.Type.Unknown
import UHF.Phases.Middle.Type.Aliases
import UHF.Phases.Middle.Type.Error

import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Control.Monad.Fix (mfix)

remove :: TypeUnknownArena -> TypedWithUnkDeclArena -> TypedWithUnkADTArena -> TypedWithUnkTypeSynonymArena -> TypedWithUnkBoundValueArena -> Compiler.WithDiagnostics Error Void (TypedDeclArena, TypedADTArena, TypedTypeSynonymArena, TypedBoundValueArena)
remove vars decls adts type_synonyms bvs =
    convert_vars vars >>= \ vars ->
    pure (Arena.transform (decl vars) decls, Arena.transform (adt vars) adts, Arena.transform (type_synonym vars) type_synonyms, Arena.transform (bound_value vars) bvs)

convert_vars :: TypeUnknownArena -> Compiler.WithDiagnostics Error Void (Arena.Arena (Maybe Type) TypeUnknownKey)
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
        r vars_converted (Type.Type'Unknown v) = MaybeT $ pure $ Arena.get vars_converted v

        convert_var vars_converted (TypeUnknown _ (Substituted s)) = r vars_converted s
        convert_var _ (TypeUnknown for_what Fresh) = lift (tell [AmbiguousType for_what]) >> MaybeT (pure Nothing)

decl :: Arena.Arena (Maybe Type) TypeUnknownKey -> TypedWithUnkDecl -> TypedDecl
decl vars (SIR.Decl'Module id nc bindings adts type_synonyms) = SIR.Decl'Module id nc (map (binding vars) bindings) adts type_synonyms
decl _ (SIR.Decl'Type ty) = SIR.Decl'Type ty

bound_value :: Arena.Arena (Maybe Type) TypeUnknownKey -> TypedWithUnkBoundValue -> TypedBoundValue
bound_value vars (SIR.BoundValue id ty sp) = SIR.BoundValue id (type_ vars ty) sp

adt :: Arena.Arena (Maybe Type) TypeUnknownKey -> TypedWithUnkADT -> TypedADT
adt vars (Type.ADT id name variants) = Type.ADT id name (map variant variants)
    where
        variant (Type.ADTVariant'Named name fields) = Type.ADTVariant'Named name (map (\ (name, ty) -> (name, type_ vars ty)) fields)
        variant (Type.ADTVariant'Anon name fields) = Type.ADTVariant'Anon name (map (type_ vars) fields)

type_synonym :: Arena.Arena (Maybe Type) TypeUnknownKey -> TypedWithUnkTypeSynonym -> TypedTypeSynonym
type_synonym vars (Type.TypeSynonym id name expansion) = Type.TypeSynonym id name (type_ vars expansion)

binding :: Arena.Arena (Maybe Type) TypeUnknownKey -> TypedWithUnkBinding -> TypedBinding
binding vars (SIR.Binding p eq_sp e) = SIR.Binding (pattern vars p) eq_sp (expr vars e)

pattern :: Arena.Arena (Maybe Type) TypeUnknownKey -> TypedWithUnkPattern -> TypedPattern
pattern vars (SIR.Pattern'Identifier ty sp bn) = SIR.Pattern'Identifier (type_ vars ty) sp bn
pattern vars (SIR.Pattern'Wildcard ty sp) = SIR.Pattern'Wildcard (type_ vars ty) sp
pattern vars (SIR.Pattern'Tuple ty sp l r) = SIR.Pattern'Tuple (type_ vars ty) sp (pattern vars l) (pattern vars r)
pattern vars (SIR.Pattern'Named ty sp at_sp bnk subpat) = SIR.Pattern'Named (type_ vars ty) sp at_sp bnk (pattern vars subpat)
pattern vars (SIR.Pattern'Poison ty sp) = SIR.Pattern'Poison (type_ vars ty) sp

expr :: Arena.Arena (Maybe Type) TypeUnknownKey -> TypedWithUnkExpr -> TypedExpr
expr vars (SIR.Expr'Identifier id ty sp bn) = SIR.Expr'Identifier id (type_ vars ty) sp bn
expr vars (SIR.Expr'Char id ty sp c) = SIR.Expr'Char id (type_ vars ty) sp c
expr vars (SIR.Expr'String id ty sp t) = SIR.Expr'String id (type_ vars ty) sp t
expr vars (SIR.Expr'Int id ty sp i) = SIR.Expr'Int id (type_ vars ty) sp i
expr vars (SIR.Expr'Float id ty sp r) = SIR.Expr'Float id (type_ vars ty) sp r
expr vars (SIR.Expr'Bool id ty sp b) = SIR.Expr'Bool id (type_ vars ty) sp b
expr vars (SIR.Expr'Tuple id ty sp l r) = SIR.Expr'Tuple id (type_ vars ty) sp (expr vars l) (expr vars r)
expr vars (SIR.Expr'Lambda id ty sp param body) = SIR.Expr'Lambda id (type_ vars ty) sp (pattern vars param) (expr vars body)
expr vars (SIR.Expr'Let id ty sp bindings result) = SIR.Expr'Let id (type_ vars ty) sp (map (binding vars) bindings) (expr vars result)
expr _ (SIR.Expr'BinaryOps _ void _ _ _ _) = absurd void
expr vars (SIR.Expr'Call id ty sp callee arg) = SIR.Expr'Call id (type_ vars ty) sp (expr vars callee) (expr vars arg)
expr vars (SIR.Expr'If id ty sp if_sp cond true false) = SIR.Expr'If id (type_ vars ty) sp if_sp (expr vars cond) (expr vars true) (expr vars false)
expr vars (SIR.Expr'Case id ty sp case_sp testing arms) = SIR.Expr'Case id (type_ vars ty) sp case_sp (expr vars testing) (map (\ (p, e) -> (pattern vars p, expr vars e)) arms)
expr vars (SIR.Expr'TypeAnnotation id ty sp annotation e) = SIR.Expr'TypeAnnotation id (type_ vars ty) sp (type_ vars annotation) (expr vars e)
expr vars (SIR.Expr'Hole id ty sp hid) = SIR.Expr'Hole id (type_ vars ty) sp hid
expr vars (SIR.Expr'Poison id ty sp) = SIR.Expr'Poison id (type_ vars ty) sp

type_ :: Arena.Arena (Maybe Type) TypeUnknownKey -> TypeWithUnk -> Maybe Type
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
        r (Type.Type'Unknown v) = Arena.get vars v
