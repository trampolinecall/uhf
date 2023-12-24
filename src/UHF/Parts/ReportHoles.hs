{-# LANGUAGE TypeOperators #-}

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

type ADTArena stage = Arena.Arena (SIR.ADT stage) Type.ADTKey
type TypeSynonymArena stage = Arena.Arena (SIR.TypeSynonym stage) Type.TypeSynonymKey
type QuantVarArena = Arena.Arena Type.QuantVar Type.QuantVarKey

data Error stage = Error (ADTArena stage) (TypeSynonymArena stage) QuantVarArena Span (Located Text) Type.Type
instance Diagnostic.ToError (Error stage) where
    to_error (Error adts type_synonyms vars sp name ty) =
        let message = "hole: '?" <> unlocate name <> "' of type '" <> PP.render (Type.PP.refer_type adts type_synonyms vars ty) <> "'"
        in Diagnostic.Error (Just sp) message [] []

report_holes :: (SIR.TypeInfo stage ~ Maybe Type.Type, SIR.TypeExprEvaledAsType stage ~ Maybe Type.Type) => SIR.SIR stage -> Compiler.WithDiagnostics (Error stage) Void ()
report_holes sir@(SIR.SIR _ _ _ _ _ _ _ mod) = runReaderT (module_ mod) sir

module_ :: (SIR.TypeInfo stage ~ Maybe Type.Type, SIR.TypeExprEvaledAsType stage ~ Maybe Type.Type) => SIR.ModuleKey -> ReaderT (SIR.SIR stage) (Compiler.WithDiagnostics (Error stage) Void) ()
module_ key =
    ask >>= \ (SIR.SIR modules _ _ _ _ _ _ _) ->
    let SIR.Module _ bindings adts type_synonyms classes instances = Arena.get modules key
    in mapM_ binding bindings >> mapM_ adt adts >> mapM_ type_synonym type_synonyms >> mapM_ class_ classes >> mapM_ instance_ instances

adt :: (SIR.TypeInfo stage ~ Maybe Type.Type, SIR.TypeExprEvaledAsType stage ~ Maybe Type.Type) => Type.ADTKey -> ReaderT (SIR.SIR stage) (Compiler.WithDiagnostics (Error stage) Void) ()
adt key = ask >>= \ (SIR.SIR _ adts _ _ _ _ _ _) -> let (Type.ADT _ _ _ variants) = Arena.get adts key in mapM_ variant variants
    where
        variant (Type.ADT.Variant'Named _ _ fields) = mapM_ (\ (_, _, (ty, _)) -> type_expr ty) fields
        variant (Type.ADT.Variant'Anon _ _ fields) = mapM_ (\ (_, (ty, _)) -> type_expr ty) fields

type_synonym :: (SIR.TypeInfo stage ~ Maybe Type.Type, SIR.TypeExprEvaledAsType stage ~ Maybe Type.Type) => Type.TypeSynonymKey -> ReaderT (SIR.SIR stage) (Compiler.WithDiagnostics (Error stage) Void) ()
type_synonym key = ask >>= \ (SIR.SIR _ _ type_synonyms _ _ _ _ _) -> let (Type.TypeSynonym _ _ (expansion, _)) = Arena.get type_synonyms key in type_expr expansion

class_ :: (SIR.TypeInfo stage ~ Maybe Type.Type, SIR.TypeExprEvaledAsType stage ~ Maybe Type.Type) => Type.ClassKey -> ReaderT (SIR.SIR stage) (Compiler.WithDiagnostics (Error stage) Void) ()
class_ key = ask >>= \ (SIR.SIR _ _ _ _ classes _ _ _) -> let (Type.Class _ _ _) = Arena.get classes key in pure ()

instance_ :: (SIR.TypeInfo stage ~ Maybe Type.Type, SIR.TypeExprEvaledAsType stage ~ Maybe Type.Type) => Type.InstanceKey -> ReaderT (SIR.SIR stage) (Compiler.WithDiagnostics (Error stage) Void) ()
instance_ key = ask >>= \ (SIR.SIR _ _ _ _ _ instances _ _) -> let (Type.Instance _ (class_, _) args) = Arena.get instances key in type_expr class_ >> mapM_ (type_expr . fst) args

binding :: (SIR.TypeInfo stage ~ Maybe Type.Type, SIR.TypeExprEvaledAsType stage ~ Maybe Type.Type) => SIR.Binding stage -> ReaderT (SIR.SIR stage) (Compiler.WithDiagnostics (Error stage) Void) ()
binding (SIR.Binding p _ e) = pattern p >> expr e

pattern :: SIR.Pattern stage -> ReaderT (SIR.SIR stage) (Compiler.WithDiagnostics (Error stage) Void) ()
pattern _ = pure () -- TODO: remove or keep for symmetry?

expr :: (SIR.TypeInfo stage ~ Maybe Type.Type, SIR.TypeExprEvaledAsType stage ~ Maybe Type.Type) => SIR.Expr stage -> ReaderT (SIR.SIR stage) (Compiler.WithDiagnostics (Error stage) Void) ()
expr (SIR.Expr'Identifier _ _ _ _ _) = pure ()
expr (SIR.Expr'Char _ _ _ _) = pure ()
expr (SIR.Expr'String _ _ _ _) = pure ()
expr (SIR.Expr'Int _ _ _ _) = pure ()
expr (SIR.Expr'Float _ _ _ _) = pure ()
expr (SIR.Expr'Bool _ _ _ _) = pure ()

expr (SIR.Expr'Tuple _ _ _ a b) = expr a >> expr b

expr (SIR.Expr'Lambda _ _ _ param body) = pattern param >> expr body

expr (SIR.Expr'Let _ _ _ bindings adts type_synonyms classes instances body) = mapM_ binding bindings >> mapM_ adt adts >> mapM_ type_synonym type_synonyms >> mapM_ class_ classes >> mapM_ instance_ instances >> expr body
expr (SIR.Expr'LetRec _ _ _ bindings adts type_synonyms classes instances body) = mapM_ binding bindings >> mapM_ adt adts >> mapM_ type_synonym type_synonyms >> mapM_ class_ classes >> mapM_ instance_ instances >> expr body

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
            ask >>= \ (SIR.SIR _ adts type_synonyms vars _ _ _ _) ->
            lift (Compiler.tell_error (Error adts type_synonyms vars sp hid type_info)) >>
            pure ()
        Nothing -> pure () -- typing phase will have already reported ambiguous type

expr (SIR.Expr'Poison _ _ _) = pure ()

type_expr :: (SIR.TypeInfo stage ~ Maybe Type.Type, SIR.TypeExprEvaledAsType stage ~ Maybe Type.Type) => SIR.TypeExpr stage -> ReaderT (SIR.SIR stage) (Compiler.WithDiagnostics (Error stage) Void) ()
type_expr (SIR.TypeExpr'Refer _ _ _) = pure ()
type_expr (SIR.TypeExpr'Get _ _ inside _) = type_expr inside
type_expr (SIR.TypeExpr'Tuple _ _ a b) = type_expr a >> type_expr b
type_expr (SIR.TypeExpr'Hole _ type_info sp hid) =
    case type_info of
        Just type_info ->
            ask >>= \ (SIR.SIR _ adts type_synonyms vars _ _ _ _) ->
            lift (Compiler.tell_error (Error adts type_synonyms vars sp hid type_info)) >>
            pure ()
        Nothing -> pure () -- typing phase will have already reported ambiguous type
type_expr (SIR.TypeExpr'Function _ _ arg res) = type_expr arg >> type_expr res
type_expr (SIR.TypeExpr'Forall _ _ _ ty) = type_expr ty
type_expr (SIR.TypeExpr'Apply _ _ ty arg) = type_expr ty >> type_expr arg
type_expr (SIR.TypeExpr'Wild _ _) = pure ()
type_expr (SIR.TypeExpr'Poison _ _) = pure ()
