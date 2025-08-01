{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module UHF.Parts.ReportHoles (report_holes) where

import UHF.Prelude

import UHF.Source.Located (Located (unlocate))
import UHF.Source.Span (Span)
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.IR.Type.PP as Type.PP
import qualified UHF.Data.SIR as SIR
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.PP as PP
import qualified UHF.Util.Arena as Arena
import qualified Data.Map as Map
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result (TypeExprsFinalEvaledAsTypes)
import UHF.Parts.UnifiedFrontendSolver.TypeSolve.Misc.Result (FinalTypeInfo (final_expr_types))

type ADTArena stage = Arena.Arena (SIR.ADT stage) Type.ADTKey
type TypeSynonymArena stage = Arena.Arena (SIR.TypeSynonym stage) Type.TypeSynonymKey
type QuantVarArena = Arena.Arena Type.QuantVar Type.QuantVarKey

data Error stage = Error (ADTArena stage) (TypeSynonymArena stage) QuantVarArena Span (Located Text) Type.Type
instance Diagnostic.ToError (Error stage) where
    to_error (Error adts type_synonyms vars sp name ty) =
        let message = "hole: '?" <> unlocate name <> "' of type '" <> PP.render (Type.PP.refer_type adts type_synonyms vars ty) <> "'"
        in Diagnostic.Error (Just sp) message [] []

report_holes :: TypeExprsFinalEvaledAsTypes -> FinalTypeInfo -> SIR.SIR stage -> Compiler.WithDiagnostics (Error stage) Void ()
report_holes type_expr_evaled_as_type_arena type_info sir@(SIR.SIR _ _ _ _ _ (SIR.CU root_module _)) = runReaderT (module_ root_module) (sir, type_expr_evaled_as_type_arena, type_info)

module_ :: SIR.ModuleKey -> ReaderT (SIR.SIR stage, TypeExprsFinalEvaledAsTypes, FinalTypeInfo) (Compiler.WithDiagnostics (Error stage) Void) ()
module_ key =
    ask >>= \ (SIR.SIR modules _ _ _ _ _, _, _) ->
    let SIR.Module _ _ _ bindings adts type_synonyms = Arena.get modules key
    in mapM_ binding bindings >> mapM_ adt adts >> mapM_ type_synonym type_synonyms

adt :: Type.ADTKey -> ReaderT (SIR.SIR stage, TypeExprsFinalEvaledAsTypes, FinalTypeInfo) (Compiler.WithDiagnostics (Error stage) Void) ()
adt key = ask >>= \ (SIR.SIR _ adts _ _ _ _, _, _) -> let (Type.ADT _ _ _ variants) = Arena.get adts key in mapM_ variant variants
    where
        variant (Type.ADT.Variant'Named _ _ fields) = mapM_ (\ (_, _, (ty, _)) -> type_expr ty) fields
        variant (Type.ADT.Variant'Anon _ _ fields) = mapM_ (\ (_, (ty, _)) -> type_expr ty) fields

type_synonym :: Type.TypeSynonymKey -> ReaderT (SIR.SIR stage, TypeExprsFinalEvaledAsTypes, FinalTypeInfo) (Compiler.WithDiagnostics (Error stage) Void) ()
type_synonym key = ask >>= \ (SIR.SIR _ _ type_synonyms _ _ _, _, _) -> let (Type.TypeSynonym _ _ (expansion, _)) = Arena.get type_synonyms key in type_expr expansion

binding :: SIR.Binding stage -> ReaderT (SIR.SIR stage, TypeExprsFinalEvaledAsTypes, FinalTypeInfo) (Compiler.WithDiagnostics (Error stage) Void) ()
binding (SIR.Binding _ p _ e) = pattern p >> expr e

pattern :: SIR.Pattern stage -> ReaderT (SIR.SIR stage, TypeExprsFinalEvaledAsTypes, FinalTypeInfo) (Compiler.WithDiagnostics (Error stage) Void) ()
pattern _ = pure () -- TODO: remove or keep for symmetry?

expr :: SIR.Expr stage -> ReaderT (SIR.SIR stage, TypeExprsFinalEvaledAsTypes, FinalTypeInfo) (Compiler.WithDiagnostics (Error stage) Void) ()
expr (SIR.Expr'Refer _ _ _ _) = pure ()
expr (SIR.Expr'Char _ _ _ _) = pure ()
expr (SIR.Expr'String _ _ _ _) = pure ()
expr (SIR.Expr'Int _ _ _ _) = pure ()
expr (SIR.Expr'Float _ _ _ _) = pure ()
expr (SIR.Expr'Bool _ _ _ _) = pure ()
expr (SIR.Expr'Tuple _ _ _ a b) = expr a >> expr b
expr (SIR.Expr'Lambda _ _ _ param body) = pattern param >> expr body
expr (SIR.Expr'Let _ _ _ _ bindings adts type_synonyms body) = mapM_ binding bindings >> mapM_ adt adts >> mapM_ type_synonym type_synonyms >> expr body
expr (SIR.Expr'LetRec _ _ _ _ bindings adts type_synonyms body) = mapM_ binding bindings >> mapM_ adt adts >> mapM_ type_synonym type_synonyms >> expr body
expr (SIR.Expr'BinaryOps _ _ _ _ first ops) = expr first >> mapM_ (\ (_, _, rhs) -> expr rhs) ops
expr (SIR.Expr'Call _ _ _ callee arg) = expr callee >> expr arg
expr (SIR.Expr'If _ _ _ _ cond t f) = expr cond >> expr t >> expr f
expr (SIR.Expr'Match _ _ _ _ e arms) = expr e >> mapM_ (\ (_, p, e) -> pattern p >> expr e) arms
expr (SIR.Expr'TypeAnnotation _ _ _ (ty, _) e) = type_expr ty >> expr e
expr (SIR.Expr'Forall _ _ _ _ _ e) = expr e
expr (SIR.Expr'TypeApply _ _ _ e (arg, _)) = expr e >> type_expr arg
expr (SIR.Expr'Hole id _ sp hiden) = do
    (SIR.SIR _ adts type_synonyms vars _ _, _, type_info) <- ask
    let ty = final_expr_types type_info Map.! id
    case ty of
        Just ty -> do
            _ <- lift (Compiler.tell_error (Error adts type_synonyms vars sp hiden ty))
            pure ()
        Nothing -> pure () -- typing phase will have already reported ambiguous type
expr (SIR.Expr'Poison _ _ _) = pure ()

type_expr :: SIR.TypeExpr stage -> ReaderT (SIR.SIR stage, TypeExprsFinalEvaledAsTypes, FinalTypeInfo) (Compiler.WithDiagnostics (Error stage) Void) ()
type_expr (SIR.TypeExpr'Refer _ _ _ _ _) = pure ()
type_expr (SIR.TypeExpr'Get _ _ _ inside _) = type_expr inside
type_expr (SIR.TypeExpr'Tuple _ _ a b) = type_expr a >> type_expr b
type_expr (SIR.TypeExpr'Hole _ hid sp hiden) = do
    (SIR.SIR _ adts type_synonyms vars _ _, type_exprs_evaled_as_types, _) <- ask
    case type_exprs_evaled_as_types Map.! hid of
        Just type_info -> do
            _ <- lift (Compiler.tell_error (Error adts type_synonyms vars sp hiden type_info))
            pure ()
        Nothing -> pure () -- typing phase will have already reported ambiguous type
type_expr (SIR.TypeExpr'Function _ _ arg res) = type_expr arg >> type_expr res
type_expr (SIR.TypeExpr'Forall _ _ _ _ ty) = type_expr ty
type_expr (SIR.TypeExpr'Apply _ _ ty arg) = type_expr ty >> type_expr arg
type_expr (SIR.TypeExpr'Wild _ _) = pure ()
type_expr (SIR.TypeExpr'Poison _ _) = pure ()
