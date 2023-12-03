module UHF.Phases.SolveTypes.RemoveInferVars (remove) where

import UHF.Prelude

import qualified Data.List.NonEmpty as NonEmpty

import UHF.Phases.SolveTypes.Aliases
import UHF.Phases.SolveTypes.Error
import UHF.Phases.SolveTypes.Solver.InferVar
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.SIR as SIR
import qualified UHF.Util.Arena as Arena

import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Control.Monad.Fix (mfix)

remove :: TypeInferVarArena -> TypedWithInferVarsModuleArena -> TypedWithInferVarsADTArena -> TypedWithInferVarsTypeSynonymArena -> TypedWithInferVarsVariableArena -> Compiler.WithDiagnostics Error Void (TypedModuleArena, TypedADTArena, TypedTypeSynonymArena, TypedVariableArena)
remove unks mods adts type_synonyms vars =
    convert_vars unks >>= \ unks ->
    pure (Arena.transform (module_ unks) mods, Arena.transform (adt unks) adts, Arena.transform (type_synonym unks) type_synonyms, Arena.transform (variable unks) vars)

convert_vars :: TypeInferVarArena -> Compiler.WithDiagnostics Error Void (Arena.Arena (Maybe Type) TypeInferVarKey)
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
        r unks_converted (Type.Type'InferVar v) = MaybeT $ pure $ Arena.get unks_converted v
        r _ (Type.Type'Variable v) = pure $ Type.Type'Variable v
        r unks_converted (Type.Type'Forall vars ty) = Type.Type'Forall vars <$> r unks_converted ty

        convert_var unks_converted (TypeInferVar _ (TSubstituted s)) = r unks_converted s
        convert_var _ (TypeInferVar for_what TFresh) = lift (Compiler.tell_error $ AmbiguousType for_what) >> MaybeT (pure Nothing)

module_ :: Arena.Arena (Maybe Type) TypeInferVarKey -> TypedWithInferVarsModule -> TypedModule
module_ unks (SIR.Module id bindings adts type_synonyms) = SIR.Module id (map (binding unks) bindings) adts type_synonyms

variable :: Arena.Arena (Maybe Type) TypeInferVarKey -> TypedWithInferVarsVariable -> TypedVariable
variable unks (SIR.Variable id ty name) = SIR.Variable id (type_ unks ty) name
variable unks (SIR.Variable'ADTVariant id index tparams ty sp) = SIR.Variable'ADTVariant id index tparams (type_ unks ty) sp

adt :: Arena.Arena (Maybe Type) TypeInferVarKey -> TypedWithInferVarsADT -> TypedADT
adt unks (Type.ADT id name type_var variants) = Type.ADT id name type_var (map variant variants)
    where
        variant (Type.ADTVariant'Named name id fields) = Type.ADTVariant'Named name id (map (\ (name, id, ty) -> (name, id, type_expr_and_type unks ty)) fields)
        variant (Type.ADTVariant'Anon name id fields) = Type.ADTVariant'Anon name id (map (\ (id, ty) -> (id, type_expr_and_type unks ty)) fields)

type_synonym :: Arena.Arena (Maybe Type) TypeInferVarKey -> TypedWithInferVarsTypeSynonym -> TypedTypeSynonym
type_synonym unks (Type.TypeSynonym id name expansion) = Type.TypeSynonym id name (type_expr_and_type unks expansion)

binding :: Arena.Arena (Maybe Type) TypeInferVarKey -> TypedWithInferVarsBinding -> TypedBinding
binding unks (SIR.Binding p eq_sp e) = SIR.Binding (pattern unks p) eq_sp (expr unks e)
binding _ (SIR.Binding'ADTVariant sp var_key vars variant) = SIR.Binding'ADTVariant sp var_key vars variant

pattern :: Arena.Arena (Maybe Type) TypeInferVarKey -> TypedWithInferVarsPattern -> TypedPattern
pattern unks (SIR.Pattern'Identifier ty sp bn) = SIR.Pattern'Identifier (type_ unks ty) sp bn
pattern unks (SIR.Pattern'Wildcard ty sp) = SIR.Pattern'Wildcard (type_ unks ty) sp
pattern unks (SIR.Pattern'Tuple ty sp l r) = SIR.Pattern'Tuple (type_ unks ty) sp (pattern unks l) (pattern unks r)
pattern unks (SIR.Pattern'Named ty sp at_sp bnk subpat) = SIR.Pattern'Named (type_ unks ty) sp at_sp bnk (pattern unks subpat)
pattern unks (SIR.Pattern'AnonADTVariant ty sp variant_iden variant_resolved tyargs fields) = SIR.Pattern'AnonADTVariant (type_ unks ty) sp (split_identifier unks variant_iden) variant_resolved (map (type_ unks) tyargs) (map (pattern unks) fields)
pattern unks (SIR.Pattern'NamedADTVariant ty sp variant_iden variant_resolved tyargs fields) = SIR.Pattern'NamedADTVariant (type_ unks ty) sp (split_identifier unks variant_iden) variant_resolved (map (type_ unks) tyargs) (map (\ (field_name, field_pat) -> (field_name, pattern unks field_pat)) fields)
pattern unks (SIR.Pattern'Poison ty sp) = SIR.Pattern'Poison (type_ unks ty) sp

expr :: Arena.Arena (Maybe Type) TypeInferVarKey -> TypedWithInferVarsExpr -> TypedExpr
expr unks (SIR.Expr'Identifier id ty sp iden_split iden_resolved) = SIR.Expr'Identifier id (type_ unks ty) sp (split_identifier unks iden_split) iden_resolved
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
expr unks (SIR.Expr'TypeAnnotation id ty sp annotation e) = SIR.Expr'TypeAnnotation id (type_ unks ty) sp (type_expr_and_type unks annotation) (expr unks e)
expr unks (SIR.Expr'Forall id ty sp names e) = SIR.Expr'Forall id (type_ unks ty) sp (NonEmpty.map identity names) (expr unks e)
expr unks (SIR.Expr'TypeApply id ty sp e args) = SIR.Expr'TypeApply id (type_ unks ty) sp (expr unks e) (type_expr_and_type unks args)
expr unks (SIR.Expr'Hole id ty sp hid) = SIR.Expr'Hole id (type_ unks ty) sp hid
expr unks (SIR.Expr'Poison id ty sp) = SIR.Expr'Poison id (type_ unks ty) sp

type_expr :: Arena.Arena (Maybe Type) TypeInferVarKey -> TypedWithInferVarsTypeExpr -> TypedTypeExpr
type_expr _ (SIR.TypeExpr'Refer evaled sp iden) = SIR.TypeExpr'Refer evaled sp iden
type_expr unks (SIR.TypeExpr'Get evaled sp parent name) = SIR.TypeExpr'Get evaled sp (type_expr unks parent) name
type_expr unks (SIR.TypeExpr'Tuple evaled sp a b) = SIR.TypeExpr'Tuple evaled sp (type_expr unks a) (type_expr unks b)
type_expr unks (SIR.TypeExpr'Hole evaled tyinfo sp hid) = SIR.TypeExpr'Hole evaled (type_ unks tyinfo) sp hid
type_expr unks (SIR.TypeExpr'Function evaled sp arg res) = SIR.TypeExpr'Function evaled sp (type_expr unks arg) (type_expr unks res)
type_expr unks (SIR.TypeExpr'Forall evaled sp names sub) = SIR.TypeExpr'Forall evaled sp names (type_expr unks sub)
type_expr unks (SIR.TypeExpr'Apply evaled sp applied_to args) = SIR.TypeExpr'Apply evaled sp (type_expr unks applied_to) (type_expr unks args)
type_expr _ (SIR.TypeExpr'Wild evaled sp) = SIR.TypeExpr'Wild evaled sp
type_expr _ (SIR.TypeExpr'Poison evaled sp) = SIR.TypeExpr'Poison evaled sp

type_expr_and_type :: Arena.Arena (Maybe Type) TypeInferVarKey -> (TypedWithInferVarsTypeExpr, TypeWithInferVars) -> (TypedTypeExpr, Maybe Type)
type_expr_and_type unks (te, t) = (type_expr unks te, type_ unks t)

split_identifier :: Arena.Arena (Maybe Type) TypeInferVarKey -> SIR.SplitIdentifier TypedWithInferVars start -> SIR.SplitIdentifier Typed start
split_identifier unks (SIR.SplitIdentifier'Get texpr next) = SIR.SplitIdentifier'Get (type_expr unks texpr) next
split_identifier _ (SIR.SplitIdentifier'Single name) = SIR.SplitIdentifier'Single name

type_ :: Arena.Arena (Maybe Type) TypeInferVarKey -> TypeWithInferVars -> Maybe Type
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
        r (Type.Type'InferVar u) = Arena.get unks u
        r (Type.Type'Variable v) = Just $ Type.Type'Variable v
        r (Type.Type'Forall vars ty) = Type.Type'Forall vars <$> r ty
