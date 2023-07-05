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

import qualified Data.Text as Text

type Type = Type.Type Void

type SIR d_iden v_iden p_iden binary_ops_allowed = SIR.SIR d_iden v_iden p_iden (Maybe Type) binary_ops_allowed
type ADT d_iden = Type.ADT (TypeExpr d_iden)
type TypeSynonym d_iden = Type.TypeSynonym (TypeExpr d_iden)
type Binding d_iden v_iden p_iden binary_ops_allowed = SIR.Binding d_iden v_iden p_iden (Maybe Type) binary_ops_allowed
type Expr d_iden v_iden p_iden binary_ops_allowed = SIR.Expr d_iden v_iden p_iden (Maybe Type) binary_ops_allowed
type Pattern p_iden = SIR.Pattern p_iden (Maybe Type)
type TypeExpr d_iden = SIR.TypeExpr d_iden (Maybe Type)

type ADTArena d_iden = Arena.Arena (ADT d_iden) Type.ADTKey
type TypeSynonymArena d_iden = Arena.Arena (TypeSynonym d_iden) Type.TypeSynonymKey
type TypeVarArena = Arena.Arena Type.Var Type.TypeVarKey

data Error d_iden = Error (ADTArena d_iden) (TypeSynonymArena d_iden) TypeVarArena Span [Located Text] Type
instance Diagnostic.ToError (Error d_iden) where
    to_error (Error adts type_synonyms vars sp name ty) =
        let message = "hole: '?" <> Text.intercalate "::" (map unlocate name) <> "' of type '" <> PP.render (Type.PP.refer_type absurd adts type_synonyms vars ty) <> "'"
        in Diagnostic.Error Diagnostic.Codes.hole (Just sp) message [] []

report_holes :: SIR d_iden v_iden p_iden binary_ops_allowed -> Compiler.WithDiagnostics (Error d_iden) Void ()
report_holes sir@(SIR.SIR _ _ _ _ _ _ mod) = runReaderT (module_ mod) sir

module_ :: SIR.ModuleKey -> ReaderT (SIR d_iden v_iden p_iden binary_ops_allowed) (Compiler.WithDiagnostics (Error d_iden) Void) ()
module_ key =
    ask >>= \ (SIR.SIR _ modules _ _ _ _ _) ->
    let SIR.Module _ bindings adts type_synonyms = Arena.get modules key
    in mapM_ binding bindings >> mapM_ adt adts >> mapM_ type_synonym type_synonyms

adt :: Type.ADTKey -> ReaderT (SIR d_iden v_iden p_iden binary_ops_allowed) (Compiler.WithDiagnostics (Error d_iden) Void) ()
adt key = ask >>= \ (SIR.SIR _ _ adts _ _ _ _) -> let (Type.ADT _ _ _ variants) = Arena.get adts key in mapM_ variant variants
    where
        variant (Type.ADTVariant'Named _ fields) = mapM_ (\ (_, ty) -> type_expr ty) fields
        variant (Type.ADTVariant'Anon _ fields) = mapM_ type_expr fields

type_synonym :: Type.TypeSynonymKey -> ReaderT (SIR d_iden v_iden p_iden binary_ops_allowed) (Compiler.WithDiagnostics (Error d_iden) Void) ()
type_synonym key = ask >>= \ (SIR.SIR _ _ _ type_synonyms _ _ _) -> let (Type.TypeSynonym _ _ expansion) = Arena.get type_synonyms key in type_expr expansion

binding :: Binding d_iden v_iden p_iden binary_ops_allowed -> ReaderT (SIR d_iden v_iden p_iden binary_ops_allowed) (Compiler.WithDiagnostics (Error d_iden) Void) ()
binding (SIR.Binding p _ e) = pattern p >> expr e
binding (SIR.Binding'ADTVariant _ _ _) = pure ()

pattern :: Pattern p_iden -> ReaderT (SIR d_iden v_iden p_iden binary_ops_allowed) (Compiler.WithDiagnostics (Error d_iden) Void) ()
pattern _ = pure () -- TODO: remove or keep for symmetry?

expr :: Expr d_iden v_iden p_iden binary_ops_allowed -> ReaderT (SIR d_iden v_iden p_iden binary_ops_allowed) (Compiler.WithDiagnostics (Error d_iden) Void) ()
expr (SIR.Expr'Identifier _ _ _ _) = pure ()
expr (SIR.Expr'Char _ _ _ _) = pure ()
expr (SIR.Expr'String _ _ _ _) = pure ()
expr (SIR.Expr'Int _ _ _ _) = pure ()
expr (SIR.Expr'Float _ _ _ _) = pure ()
expr (SIR.Expr'Bool _ _ _ _) = pure ()

expr (SIR.Expr'Tuple _ _ _ a b) = expr a >> expr b

expr (SIR.Expr'Lambda _ _ _ param body) = pattern param >> expr body

expr (SIR.Expr'Let _ _ _ bindings body) = mapM_ binding bindings >> expr body
expr (SIR.Expr'LetRec _ _ _ bindings body) = mapM_ binding bindings >> expr body

expr (SIR.Expr'BinaryOps _ _ _ _ first ops) = expr first >> mapM_ (\ (_, rhs) -> expr rhs) ops

expr (SIR.Expr'Call _ _ _ callee arg) = expr callee >> expr arg

expr (SIR.Expr'If _ _ _ _ cond t f) = expr cond >> expr t >> expr f
expr (SIR.Expr'Case _ _ _ _ e arms) = expr e >> mapM_ (\ (p, e) -> pattern p >> expr e) arms

expr (SIR.Expr'TypeAnnotation _ _ _ ty e) = type_expr ty >> expr e

expr (SIR.Expr'Forall _ _ _ _ e) = expr e
expr (SIR.Expr'TypeApply _ _ _ e args) = expr e >> type_expr args

expr (SIR.Expr'Hole _ type_info sp hid) =
    case type_info of
        Just type_info ->
            ask >>= \ (SIR.SIR _ _ adts type_synonyms vars _ _) ->
            lift (Compiler.tell_error (Error adts type_synonyms vars sp (unlocate hid) type_info))
        Nothing -> pure () -- typing phase will have already reported ambiguous type

expr (SIR.Expr'Poison _ _ _) = pure ()

type_expr :: TypeExpr d_iden -> ReaderT (SIR d_iden v_iden p_iden binary_ops_allowed) (Compiler.WithDiagnostics (Error d_iden) Void) ()
type_expr (SIR.TypeExpr'Identifier _ _ _) = pure ()
type_expr (SIR.TypeExpr'Tuple _ a b) = type_expr a >> type_expr b
type_expr (SIR.TypeExpr'Hole type_info sp hid) =
    case type_info of
        Just type_info ->
            ask >>= \ (SIR.SIR _ _ adts type_synonyms vars _ _) ->
            lift (Compiler.tell_error (Error adts type_synonyms vars sp (unlocate hid) type_info))
        Nothing -> pure () -- typing phase will have already reported ambiguous type
type_expr (SIR.TypeExpr'Function _ _ arg res) = type_expr arg >> type_expr res
type_expr (SIR.TypeExpr'Forall _ _ ty) = type_expr ty
type_expr (SIR.TypeExpr'Apply _ _ ty arg) = type_expr ty >> type_expr arg
type_expr (SIR.TypeExpr'Wild _ _) = pure ()
type_expr (SIR.TypeExpr'Poison _ _) = pure ()
