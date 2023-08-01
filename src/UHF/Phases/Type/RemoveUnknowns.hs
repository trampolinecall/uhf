module UHF.Phases.Type.RemoveUnknowns (remove) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type

import qualified UHF.Compiler as Compiler

import UHF.Phases.Type.Unknown
import UHF.Phases.Type.Aliases
import UHF.Phases.Type.Error

import qualified Data.List.NonEmpty as NonEmpty

import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Control.Monad.Fix (mfix)

remove :: TypeUnknownArena -> TypedWithUnkModuleArena -> TypedWithUnkADTArena -> TypedWithUnkTypeSynonymArena -> TypedWithUnkBoundValueArena -> Compiler.WithDiagnostics Error Void (TypedModuleArena, TypedADTArena, TypedTypeSynonymArena, TypedBoundValueArena)
remove unks mods adts type_synonyms bvs =
    convert_vars unks >>= \ unks ->
    pure (Arena.transform (module_ unks) mods, Arena.transform (adt unks) adts, Arena.transform (type_synonym unks) type_synonyms, Arena.transform (bound_value unks) bvs)

convert_vars :: TypeUnknownArena -> Compiler.WithDiagnostics Error Void (Arena.Arena (Maybe Type) TypeUnknownKey)
convert_vars unks =
    -- infinite recursion is not possible because occurs check prevents loops in substitution
    mfix (\ unks_converted -> Arena.transformM (runMaybeT . convert_var unks_converted) unks)
    where
        r _ Type.Type'Int = pure Type.Type'Int
        r _ Type.Type'Float = pure Type.Type'Float
        r _ Type.Type'Char = pure Type.Type'Char
        r _ Type.Type'String = pure Type.Type'String
        r _ Type.Type'Bool = pure Type.Type'Bool
        r unks_converted (Type.Type'ADT a params) = Type.Type'ADT a <$> mapM (r unks_converted) params
        r _ (Type.Type'Synonym s) = pure $ Type.Type'Synonym s
        r unks_converted (Type.Type'Function arg res) = Type.Type'Function <$> r unks_converted arg <*> r unks_converted res
        r unks_converted (Type.Type'Tuple a b) = Type.Type'Tuple <$> r unks_converted a <*> r unks_converted b
        r unks_converted (Type.Type'Unknown v) = MaybeT $ pure $ Arena.get unks_converted v
        r _ (Type.Type'Variable v) = pure $ Type.Type'Variable v
        r unks_converted (Type.Type'Forall vars ty) = Type.Type'Forall vars <$> r unks_converted ty

        convert_var unks_converted (TypeUnknown _ (Substituted s)) = r unks_converted s
        convert_var _ (TypeUnknown for_what Fresh) = lift (Compiler.tell_error $ AmbiguousType for_what) >> MaybeT (pure Nothing)

module_ :: Arena.Arena (Maybe Type) TypeUnknownKey -> TypedWithUnkModule -> TypedModule
module_ unks (SIR.Module id bindings adts type_synonyms) = SIR.Module id (map (binding unks) bindings) adts type_synonyms

bound_value :: Arena.Arena (Maybe Type) TypeUnknownKey -> TypedWithUnkBoundValue -> TypedBoundValue
bound_value unks (SIR.BoundValue id ty name) = SIR.BoundValue id (type_ unks ty) name
bound_value unks (SIR.BoundValue'ADTVariant id index tparams ty sp) = SIR.BoundValue'ADTVariant id index tparams (type_ unks ty) sp

adt :: Arena.Arena (Maybe Type) TypeUnknownKey -> TypedWithUnkADT -> TypedADT
adt unks (Type.ADT id name type_var variants) = Type.ADT id name type_var (map variant variants)
    where
        variant (Type.ADTVariant'Named name id fields) = Type.ADTVariant'Named name id (map (\ (name, id, ty) -> (name, id, type_expr unks ty)) fields)
        variant (Type.ADTVariant'Anon name id fields) = Type.ADTVariant'Anon name id (map (\ (id, ty) -> (id, type_expr unks ty)) fields)

type_synonym :: Arena.Arena (Maybe Type) TypeUnknownKey -> TypedWithUnkTypeSynonym -> TypedTypeSynonym
type_synonym unks (Type.TypeSynonym id name expansion) = Type.TypeSynonym id name (type_expr unks expansion)

binding :: Arena.Arena (Maybe Type) TypeUnknownKey -> TypedWithUnkBinding -> TypedBinding
binding unks (SIR.Binding p eq_sp e) = SIR.Binding (pattern unks p) eq_sp (expr unks e)
binding _ (SIR.Binding'ADTVariant sp bvk vars variant) = SIR.Binding'ADTVariant sp bvk vars variant

pattern :: Arena.Arena (Maybe Type) TypeUnknownKey -> TypedWithUnkPattern -> TypedPattern
pattern unks (SIR.Pattern'Identifier ty sp bn) = SIR.Pattern'Identifier (type_ unks ty) sp bn
pattern unks (SIR.Pattern'Wildcard ty sp) = SIR.Pattern'Wildcard (type_ unks ty) sp
pattern unks (SIR.Pattern'Tuple ty sp l r) = SIR.Pattern'Tuple (type_ unks ty) sp (pattern unks l) (pattern unks r)
pattern unks (SIR.Pattern'Named ty sp at_sp bnk subpat) = SIR.Pattern'Named (type_ unks ty) sp at_sp bnk (pattern unks subpat)
pattern unks (SIR.Pattern'AnonADTVariant ty sp variant tyargs fields) = SIR.Pattern'AnonADTVariant (type_ unks ty) sp variant (map (type_ unks) tyargs) (map (pattern unks) fields)
pattern unks (SIR.Pattern'NamedADTVariant ty sp variant tyargs fields) = SIR.Pattern'NamedADTVariant (type_ unks ty) sp variant (map (type_ unks) tyargs) (map (\ (field_name, field_pat) -> (field_name, pattern unks field_pat)) fields)
pattern unks (SIR.Pattern'Poison ty sp) = SIR.Pattern'Poison (type_ unks ty) sp

expr :: Arena.Arena (Maybe Type) TypeUnknownKey -> TypedWithUnkExpr -> TypedExpr
expr unks (SIR.Expr'Identifier id ty sp bn) = SIR.Expr'Identifier id (type_ unks ty) sp bn
expr unks (SIR.Expr'Char id ty sp c) = SIR.Expr'Char id (type_ unks ty) sp c
expr unks (SIR.Expr'String id ty sp t) = SIR.Expr'String id (type_ unks ty) sp t
expr unks (SIR.Expr'Int id ty sp i) = SIR.Expr'Int id (type_ unks ty) sp i
expr unks (SIR.Expr'Float id ty sp r) = SIR.Expr'Float id (type_ unks ty) sp r
expr unks (SIR.Expr'Bool id ty sp b) = SIR.Expr'Bool id (type_ unks ty) sp b
expr unks (SIR.Expr'Tuple id ty sp l r) = SIR.Expr'Tuple id (type_ unks ty) sp (expr unks l) (expr unks r)
expr unks (SIR.Expr'Lambda id ty sp param body) = SIR.Expr'Lambda id (type_ unks ty) sp (pattern unks param) (expr unks body)
expr unks (SIR.Expr'Let id ty sp bindings result) = SIR.Expr'Let id (type_ unks ty) sp (map (binding unks) bindings) (expr unks result)
expr unks (SIR.Expr'LetRec id ty sp bindings result) = SIR.Expr'LetRec id (type_ unks ty) sp (map (binding unks) bindings) (expr unks result)
expr _ (SIR.Expr'BinaryOps _ void _ _ _ _) = absurd void
expr unks (SIR.Expr'Call id ty sp callee arg) = SIR.Expr'Call id (type_ unks ty) sp (expr unks callee) (expr unks arg)
expr unks (SIR.Expr'If id ty sp if_sp cond true false) = SIR.Expr'If id (type_ unks ty) sp if_sp (expr unks cond) (expr unks true) (expr unks false)
expr unks (SIR.Expr'Match id ty sp match_tok_sp testing arms) = SIR.Expr'Match id (type_ unks ty) sp match_tok_sp (expr unks testing) (map (\ (p, e) -> (pattern unks p, expr unks e)) arms)
expr unks (SIR.Expr'TypeAnnotation id ty sp annotation e) = SIR.Expr'TypeAnnotation id (type_ unks ty) sp (type_expr unks annotation) (expr unks e)
expr unks (SIR.Expr'Forall id ty sp names e) = SIR.Expr'Forall id (type_ unks ty) sp (NonEmpty.map identity names) (expr unks e)
expr unks (SIR.Expr'TypeApply id ty sp e args) = SIR.Expr'TypeApply id (type_ unks ty) sp (expr unks e) (type_expr unks args)
expr unks (SIR.Expr'Hole id ty sp hid) = SIR.Expr'Hole id (type_ unks ty) sp hid
expr unks (SIR.Expr'Poison id ty sp) = SIR.Expr'Poison id (type_ unks ty) sp

type_expr :: Arena.Arena (Maybe Type) TypeUnknownKey -> TypedWithUnkTypeExpr -> TypedTypeExpr
type_expr unks (SIR.TypeExpr'Identifier ty sp iden) = SIR.TypeExpr'Identifier (type_ unks ty) sp iden
type_expr unks (SIR.TypeExpr'Tuple ty a b) = SIR.TypeExpr'Tuple (type_ unks ty) (type_expr unks a) (type_expr unks b)
type_expr unks (SIR.TypeExpr'Hole ty sp hid) = SIR.TypeExpr'Hole (type_ unks ty) sp hid
type_expr unks (SIR.TypeExpr'Function ty sp arg res) = SIR.TypeExpr'Function (type_ unks ty) sp (type_expr unks arg) (type_expr unks res)
type_expr unks (SIR.TypeExpr'Forall ty names sub) = SIR.TypeExpr'Forall (type_ unks ty) names (type_expr unks sub)
type_expr unks (SIR.TypeExpr'Apply ty sp applied_to args) = SIR.TypeExpr'Apply (type_ unks ty) sp (type_expr unks applied_to) (type_expr unks args)
type_expr unks (SIR.TypeExpr'Wild ty sp) = SIR.TypeExpr'Wild (type_ unks ty) sp
type_expr unks (SIR.TypeExpr'Poison ty sp) = SIR.TypeExpr'Poison (type_ unks ty) sp

type_ :: Arena.Arena (Maybe Type) TypeUnknownKey -> TypeWithUnk -> Maybe Type
type_ unks = r
    where
        r Type.Type'Int = pure Type.Type'Int
        r Type.Type'Float = pure Type.Type'Float
        r Type.Type'Char = pure Type.Type'Char
        r Type.Type'String = pure Type.Type'String
        r Type.Type'Bool = pure Type.Type'Bool
        r (Type.Type'ADT a params) = Type.Type'ADT a <$> mapM r params
        r (Type.Type'Synonym s) = pure $ Type.Type'Synonym s
        r (Type.Type'Function arg res) = Type.Type'Function <$> r arg <*> r res
        r (Type.Type'Tuple a b) = Type.Type'Tuple <$> r a <*> r b
        r (Type.Type'Unknown u) = Arena.get unks u
        r (Type.Type'Variable v) = Just $ Type.Type'Variable v
        r (Type.Type'Forall vars ty) = Type.Type'Forall vars <$> r ty
