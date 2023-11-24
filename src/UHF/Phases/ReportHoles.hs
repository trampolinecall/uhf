module UHF.Phases.ReportHoles (report_holes) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.Compiler as Compiler

import qualified UHF.PP as PP

import UHF.IO.Located (Located (unlocate))
import UHF.IO.Span (Span)

import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Diagnostic.Codes

import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.PP as Type.PP

type Type = Type.Type Void

-- TODO: remove all of these aliases
type SIR stage = SIR.SIR stage
type ADT stage = Type.ADT (TypeExpr stage, SIR.TypeExprEvaledAsType stage)
type TypeSynonym stage = Type.TypeSynonym (TypeExpr stage, SIR.TypeExprEvaledAsType stage)
type Binding stage = SIR.Binding stage
type Expr stage = SIR.Expr stage
type Pattern stage = SIR.Pattern stage
type TypeExpr stage = SIR.TypeExpr stage

type ADTArena stage = Arena.Arena (ADT stage) Type.ADTKey
type TypeSynonymArena stage = Arena.Arena (TypeSynonym stage) Type.TypeSynonymKey
type TypeVarArena = Arena.Arena Type.Var Type.TypeVarKey

data Error stage = Error (ADTArena stage) (TypeSynonymArena stage) TypeVarArena Span (Located Text) Type
instance Diagnostic.ToError (Error stage) where
    to_error (Error adts type_synonyms vars sp name ty) =
        let message = "hole: '?" <> unlocate name <> "' of type '" <> PP.render (Type.PP.refer_type absurd adts type_synonyms vars ty) <> "'"
        in Diagnostic.Error Diagnostic.Codes.hole (Just sp) message [] []

report_holes :: (SIR.TypeInfo stage ~ Maybe Type, SIR.TypeExprEvaledAsType stage ~ Maybe Type) => SIR stage -> Compiler.WithDiagnostics (Error stage) Void ()
report_holes sir@(SIR.SIR _ _ _ _ _ _ mod) = runReaderT (module_ mod) sir

module_ :: (SIR.TypeInfo stage ~ Maybe Type, SIR.TypeExprEvaledAsType stage ~ Maybe Type) => SIR.ModuleKey -> ReaderT (SIR stage) (Compiler.WithDiagnostics (Error stage) Void) ()
module_ key =
    ask >>= \ (SIR.SIR _ modules _ _ _ _ _) ->
    let SIR.Module _ bindings adts type_synonyms = Arena.get modules key
    in mapM_ binding bindings >> mapM_ adt adts >> mapM_ type_synonym type_synonyms

adt :: (SIR.TypeInfo stage ~ Maybe Type, SIR.TypeExprEvaledAsType stage ~ Maybe Type) => Type.ADTKey -> ReaderT (SIR stage) (Compiler.WithDiagnostics (Error stage) Void) ()
adt key = ask >>= \ (SIR.SIR _ _ adts _ _ _ _) -> let (Type.ADT _ _ _ variants) = Arena.get adts key in mapM_ variant variants
    where
        variant (Type.ADTVariant'Named _ _ fields) = mapM_ (\ (_, _, (ty, _)) -> type_expr ty) fields
        variant (Type.ADTVariant'Anon _ _ fields) = mapM_ (\ (_, (ty, _)) -> type_expr ty) fields

type_synonym :: (SIR.TypeInfo stage ~ Maybe Type, SIR.TypeExprEvaledAsType stage ~ Maybe Type) => Type.TypeSynonymKey -> ReaderT (SIR stage) (Compiler.WithDiagnostics (Error stage) Void) ()
type_synonym key = ask >>= \ (SIR.SIR _ _ _ type_synonyms _ _ _) -> let (Type.TypeSynonym _ _ (expansion, _)) = Arena.get type_synonyms key in type_expr expansion

binding :: (SIR.TypeInfo stage ~ Maybe Type, SIR.TypeExprEvaledAsType stage ~ Maybe Type) => Binding stage -> ReaderT (SIR stage) (Compiler.WithDiagnostics (Error stage) Void) ()
binding (SIR.Binding p _ e) = pattern p >> expr e
binding (SIR.Binding'ADTVariant _ _ _ _) = pure ()

pattern :: Pattern p_iden -> ReaderT (SIR stage) (Compiler.WithDiagnostics (Error stage) Void) ()
pattern _ = pure () -- TODO: remove or keep for symmetry?

expr :: (SIR.TypeInfo stage ~ Maybe Type, SIR.TypeExprEvaledAsType stage ~ Maybe Type) => Expr stage -> ReaderT (SIR stage) (Compiler.WithDiagnostics (Error stage) Void) ()
expr (SIR.Expr'Identifier _ _ _ _ _) = pure ()
expr (SIR.Expr'Char _ _ _ _) = pure ()
expr (SIR.Expr'String _ _ _ _) = pure ()
expr (SIR.Expr'Int _ _ _ _) = pure ()
expr (SIR.Expr'Float _ _ _ _) = pure ()
expr (SIR.Expr'Bool _ _ _ _) = pure ()

expr (SIR.Expr'Tuple _ _ _ a b) = expr a >> expr b

expr (SIR.Expr'Lambda _ _ _ param body) = pattern param >> expr body

expr (SIR.Expr'Let _ _ _ bindings body) = mapM_ binding bindings >> expr body
expr (SIR.Expr'LetRec _ _ _ bindings body) = mapM_ binding bindings >> expr body

expr (SIR.Expr'BinaryOps _ _ _ _ first ops) = expr first >> mapM_ (\ (_, _, _, rhs) -> expr rhs) ops

expr (SIR.Expr'Call _ _ _ callee arg) = expr callee >> expr arg

expr (SIR.Expr'If _ _ _ _ cond t f) = expr cond >> expr t >> expr f
expr (SIR.Expr'Match _ _ _ _ e arms) = expr e >> mapM_ (\ (p, e) -> pattern p >> expr e) arms

expr (SIR.Expr'TypeAnnotation _ _ _ (ty, _) e) = type_expr ty >> expr e

expr (SIR.Expr'Forall _ _ _ _ e) = expr e
expr (SIR.Expr'TypeApply _ _ _ e (arg, _)) = expr e >> type_expr arg

expr (SIR.Expr'Hole _ type_info sp hid) =
    case type_info of
        Just type_info ->
            ask >>= \ (SIR.SIR _ _ adts type_synonyms vars _ _) ->
            lift (Compiler.tell_error (Error adts type_synonyms vars sp hid type_info))
        Nothing -> pure () -- typing phase will have already reported ambiguous type

expr (SIR.Expr'Poison _ _ _) = pure ()

type_expr :: (SIR.TypeInfo stage ~ Maybe Type, SIR.TypeExprEvaledAsType stage ~ Maybe Type) => TypeExpr stage -> ReaderT (SIR stage) (Compiler.WithDiagnostics (Error stage) Void) ()
type_expr (SIR.TypeExpr'Refer _ _ _) = pure ()
type_expr (SIR.TypeExpr'Get _ _ inside _) = type_expr inside
type_expr (SIR.TypeExpr'Tuple _ _ a b) = type_expr a >> type_expr b
type_expr (SIR.TypeExpr'Hole _ type_info sp hid) =
    case type_info of
        Just type_info ->
            ask >>= \ (SIR.SIR _ _ adts type_synonyms vars _ _) ->
            lift (Compiler.tell_error (Error adts type_synonyms vars sp hid type_info))
        Nothing -> pure () -- typing phase will have already reported ambiguous type
type_expr (SIR.TypeExpr'Function _ _ arg res) = type_expr arg >> type_expr res
type_expr (SIR.TypeExpr'Forall _ _ _ ty) = type_expr ty
type_expr (SIR.TypeExpr'Apply _ _ ty arg) = type_expr ty >> type_expr arg
type_expr (SIR.TypeExpr'Wild _ _) = pure ()
type_expr (SIR.TypeExpr'Poison _ _) = pure ()
