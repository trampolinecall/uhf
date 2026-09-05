{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

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
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result (TypeExprEvaledAsTypeKey)
import UHF.Data.SIR.Visitor
import Data.Data (Proxy (..))

type ADTArena stage = Arena.Arena (SIR.ADT stage) Type.ADTKey
type TypeSynonymArena stage = Arena.Arena (SIR.TypeSynonym stage) Type.TypeSynonymKey
type QuantVarArena = Arena.Arena Type.QuantVar Type.QuantVarKey
type TypeExprEvaledAsTypeArena = Arena.Arena (Maybe Type.Type) TypeExprEvaledAsTypeKey

data Error stage = Error (ADTArena stage) (TypeSynonymArena stage) QuantVarArena Span (Located Text) Type.Type
instance Diagnostic.ToError (Error stage) where
    to_error (Error adts type_synonyms vars sp name ty) =
        let message = "hole: '?" <> unlocate name <> "' of type '" <> PP.render (Type.PP.refer_type adts type_synonyms vars ty) <> "'"
        in Diagnostic.Error (Just sp) message [] []

data ReportHolesVisitor stage

report_holes :: (SIR.TypeInfo stage ~ Maybe Type.Type, SIR.TypeExprEvaledAsTypeKey stage ~ TypeExprEvaledAsTypeKey) => Arena.Arena (Maybe Type.Type) TypeExprEvaledAsTypeKey -> SIR.SIR stage -> Compiler.WithDiagnostics (Error stage) Void ()
report_holes type_expr_evaled_as_type_arena sir@(SIR.SIR _ _ _ _ _ (SIR.CU root_module _)) = runReaderT (module_ root_module) (sir, type_expr_evaled_as_type_arena)

-- TODO: if i remove arenas then i can put an SIRVisitor here instead of having this module_ function (i could probably also do that now anyways with a custom CUVisitor instance?)

module_ :: (SIR.TypeInfo stage ~ Maybe Type.Type, SIR.TypeExprEvaledAsTypeKey stage ~ TypeExprEvaledAsTypeKey) => SIR.ModuleKey -> ReaderT (SIR.SIR stage, TypeExprEvaledAsTypeArena) (Compiler.WithDiagnostics (Error stage) Void) ()
module_ key =
    ask >>= \ (SIR.SIR modules _ _ _ _ _, _) ->
    let SIR.Module _ _ bindings adts type_synonyms = Arena.get modules key
    in mapM_ (visit_binding' (Proxy :: Proxy (ReportHolesVisitor stage))) bindings >> mapM_ adt adts >> mapM_ type_synonym type_synonyms

adt :: (SIR.TypeInfo stage ~ Maybe Type.Type, SIR.TypeExprEvaledAsTypeKey stage ~ TypeExprEvaledAsTypeKey) => Type.ADTKey -> ReaderT (SIR.SIR stage, TypeExprEvaledAsTypeArena) (Compiler.WithDiagnostics (Error stage) Void) ()
adt key = ask >>= \ (SIR.SIR _ adts _ _ _ _, _) -> let (Type.ADT _ _ _ variants) = Arena.get adts key in mapM_ variant variants
    where
        variant (Type.ADT.Variant'Named _ _ fields) = mapM_ (\ (_, _, (ty, _)) -> visit_type_expr' (Proxy :: Proxy (ReportHolesVisitor stage)) ty) fields
        variant (Type.ADT.Variant'Anon _ _ fields) = mapM_ (\ (_, (ty, _)) -> visit_type_expr' (Proxy :: Proxy (ReportHolesVisitor stage)) ty) fields

type_synonym :: (SIR.TypeInfo stage ~ Maybe Type.Type, SIR.TypeExprEvaledAsTypeKey stage ~ TypeExprEvaledAsTypeKey) => Type.TypeSynonymKey -> ReaderT (SIR.SIR stage, TypeExprEvaledAsTypeArena) (Compiler.WithDiagnostics (Error stage) Void) ()
type_synonym key = ask >>= \ (SIR.SIR _ _ type_synonyms _ _ _, _) -> let (Type.TypeSynonym _ _ (expansion, _)) = Arena.get type_synonyms key in visit_type_expr' (Proxy :: Proxy (ReportHolesVisitor stage)) expansion >> pure ()

instance TransformsNameMapIndex stage stage () (ReaderT (SIR.SIR stage, TypeExprEvaledAsTypeArena) (Compiler.WithDiagnostics (Error stage) Void)) (ReportHolesVisitor stage) where
instance TransformsIdenResolvedKey stage stage (SIR.DeclRef drty) (SIR.DeclRef drty) () (ReaderT (SIR.SIR stage, TypeExprEvaledAsTypeArena) (Compiler.WithDiagnostics (Error stage) Void)) (ReportHolesVisitor stage) where
instance TransformsIdenResolvedKey stage stage SIR.ValueRef SIR.ValueRef () (ReaderT (SIR.SIR stage, TypeExprEvaledAsTypeArena) (Compiler.WithDiagnostics (Error stage) Void)) (ReportHolesVisitor stage) where
instance TransformsIdenResolvedKey stage stage Type.ADT.VariantIndex Type.ADT.VariantIndex () (ReaderT (SIR.SIR stage, TypeExprEvaledAsTypeArena) (Compiler.WithDiagnostics (Error stage) Void)) (ReportHolesVisitor stage) where
instance TransformsTypeInRefer stage stage () (ReaderT (SIR.SIR stage, TypeExprEvaledAsTypeArena) (Compiler.WithDiagnostics (Error stage) Void)) (ReportHolesVisitor stage) where
instance TransformsTypeExprEvaledKey stage stage () (ReaderT (SIR.SIR stage, TypeExprEvaledAsTypeArena) (Compiler.WithDiagnostics (Error stage) Void)) (ReportHolesVisitor stage) where
instance TransformsTypeExprEvaledAsTypeKey stage stage () (ReaderT (SIR.SIR stage, TypeExprEvaledAsTypeArena) (Compiler.WithDiagnostics (Error stage) Void)) (ReportHolesVisitor stage) where
instance TransformsTypeInfo stage stage () (ReaderT (SIR.SIR stage, TypeExprEvaledAsTypeArena) (Compiler.WithDiagnostics (Error stage) Void)) (ReportHolesVisitor stage) where
instance TransformsInfixGroupedKey stage stage () (ReaderT (SIR.SIR stage, TypeExprEvaledAsTypeArena) (Compiler.WithDiagnostics (Error stage) Void)) (ReportHolesVisitor stage) where

instance (SIR.TypeInfo stage ~ Maybe Type.Type, SIR.TypeExprEvaledAsTypeKey stage ~ TypeExprEvaledAsTypeKey) => BindingVisitor stage stage () (ReaderT (SIR.SIR stage, TypeExprEvaledAsTypeArena) (Compiler.WithDiagnostics (Error stage) Void)) () (ReportHolesVisitor stage) where

instance (SIR.TypeInfo stage ~ Maybe Type.Type, SIR.TypeExprEvaledAsTypeKey stage ~ TypeExprEvaledAsTypeKey) => TypeExprVisitor stage stage () (ReaderT (SIR.SIR stage, TypeExprEvaledAsTypeArena) (Compiler.WithDiagnostics (Error stage) Void)) () (ReportHolesVisitor stage) where
    visit_type_expr_hole _ _ tyek tyeat sp hid = do
        (_, type_expr_evaled_as_type_arena) <- ask
        case Arena.get type_expr_evaled_as_type_arena tyeat of
            Just type_info ->
                ask >>= \ (SIR.SIR _ adts type_synonyms vars _ _, _) ->
                lift (Compiler.tell_error (Error adts type_synonyms vars sp hid type_info)) >>
                pure (SIR.TypeExpr'Hole tyek tyeat sp hid, ())
            Nothing -> pure (SIR.TypeExpr'Hole tyek tyeat sp hid, ()) -- typing phase will have already reported ambiguous type

instance (SIR.TypeInfo stage ~ Maybe Type.Type, SIR.TypeExprEvaledAsTypeKey stage ~ TypeExprEvaledAsTypeKey) => ExprIdentifierRefVisitor stage stage () (ReaderT (SIR.SIR stage, TypeExprEvaledAsTypeArena) (Compiler.WithDiagnostics (Error stage) Void)) () (ReportHolesVisitor stage) where
instance (SIR.TypeInfo stage ~ Maybe Type.Type, SIR.TypeExprEvaledAsTypeKey stage ~ TypeExprEvaledAsTypeKey) => OperatorRefVisitor stage stage () (ReaderT (SIR.SIR stage, TypeExprEvaledAsTypeArena) (Compiler.WithDiagnostics (Error stage) Void)) () (ReportHolesVisitor stage) where
instance (SIR.TypeInfo stage ~ Maybe Type.Type, SIR.TypeExprEvaledAsTypeKey stage ~ TypeExprEvaledAsTypeKey) => ExprVisitor stage stage () (ReaderT (SIR.SIR stage, TypeExprEvaledAsTypeArena) (Compiler.WithDiagnostics (Error stage) Void)) () (ReportHolesVisitor stage) where
    visit_expr_let proxy () id type_info span name_map_index bindings adts type_synonyms body = do
        mapM_ (visit_binding proxy ()) bindings
        mapM_ adt adts
        mapM_ type_synonym type_synonyms
        (_, ()) <- visit_expr proxy () body
        pure (SIR.Expr'Let id type_info span name_map_index bindings adts type_synonyms body, ())

    visit_expr_let_rec proxy () id type_info span name_map_index bindings adts type_synonyms body = do
        mapM_ (visit_binding proxy ()) bindings
        mapM_ adt adts
        mapM_ type_synonym type_synonyms
        (_, ()) <- visit_expr proxy () body
        pure (SIR.Expr'LetRec id type_info span name_map_index bindings adts type_synonyms body, ())

    visit_expr_hole _ _ id type_info sp hid =
        case type_info of
            Just type_info' ->
                ask >>= \ (SIR.SIR _ adts type_synonyms vars _ _, _) ->
                lift (Compiler.tell_error (Error adts type_synonyms vars sp hid type_info')) >>
                pure (SIR.Expr'Hole id type_info sp hid, ())
            Nothing -> pure (SIR.Expr'Hole id type_info sp hid, ()) -- typing phase will have already reported ambiguous type

instance (SIR.TypeInfo stage ~ Maybe Type.Type, SIR.TypeExprEvaledAsTypeKey stage ~ TypeExprEvaledAsTypeKey) => PatternADTVariantRefVisitor stage stage () (ReaderT (SIR.SIR stage, TypeExprEvaledAsTypeArena) (Compiler.WithDiagnostics (Error stage) Void)) () (ReportHolesVisitor stage) where
instance (SIR.TypeInfo stage ~ Maybe Type.Type, SIR.TypeExprEvaledAsTypeKey stage ~ TypeExprEvaledAsTypeKey) => PatternVisitor stage stage () (ReaderT (SIR.SIR stage, TypeExprEvaledAsTypeArena) (Compiler.WithDiagnostics (Error stage) Void)) () (ReportHolesVisitor stage) where
