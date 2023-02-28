module UHF.Phases.Middle.ToRIR (convert) where

import UHF.Util.Prelude

import qualified Arena

import UHF.IO.Span (Span)
import UHF.IO.Located (Located (unlocate))

import qualified UHF.Compiler as Compiler

import qualified UHF.Data.IR.HIR as HIR
import qualified UHF.Data.IR.RIR as RIR
import qualified UHF.Data.IR.Type as Type
import UHF.Data.IR.Keys

type Type = Maybe (Type.Type Void)

type HIRDecl = HIR.Decl (Located (Maybe BoundValueKey)) Type Type Void
type HIRExpr = HIR.Expr (Located (Maybe BoundValueKey)) Type Type Void
type HIRPattern = HIR.Pattern (Located (Maybe BoundValueKey)) Type
type HIRBinding = HIR.Binding (Located (Maybe BoundValueKey)) Type Type Void

type RIRDecl = RIR.Decl
type RIRExpr = RIR.Expr
type RIRBinding = RIR.Binding

type HIRDeclArena = Arena.Arena HIRDecl DeclKey
type BoundValueArena = Arena.Arena (HIR.BoundValue Type) BoundValueKey

type RIRDeclArena = Arena.Arena RIRDecl DeclKey

type ConvertState = StateT BoundValueArena Compiler.Compiler

convert :: HIRDeclArena -> BoundValueArena -> Compiler.Compiler (RIRDeclArena, BoundValueArena)
convert decls bvs =
    runStateT (Arena.transformM convert_decl decls) bvs >>= \ (decls, bvs) ->
    pure (decls, bvs)

convert_decl :: HIRDecl -> ConvertState RIRDecl
convert_decl (HIR.Decl'Module _ bindings) = RIR.Decl'Module <$> (concat <$> mapM convert_binding bindings)
convert_decl (HIR.Decl'Type ty) = pure $ RIR.Decl'Type ty

convert_binding :: HIRBinding -> ConvertState [RIRBinding]
convert_binding (HIR.Binding pat _ expr) = convert_expr expr >>= assign_pattern pat

new_bound_value :: Type -> Span -> ConvertState BoundValueKey
new_bound_value ty sp = state (Arena.put (HIR.BoundValue ty sp))

convert_expr :: HIRExpr -> ConvertState RIRExpr
convert_expr (HIR.Expr'Identifier ty sp bv) = pure $ RIR.Expr'Identifier ty sp (unlocate bv)
convert_expr (HIR.Expr'Char ty sp c) = pure $ RIR.Expr'Char ty sp c
convert_expr (HIR.Expr'String ty sp s) = pure $ RIR.Expr'String ty sp s
convert_expr (HIR.Expr'Int ty sp i) = pure $ RIR.Expr'Int ty sp i
convert_expr (HIR.Expr'Float ty sp f) = pure $ RIR.Expr'Float ty sp f
convert_expr (HIR.Expr'Bool ty sp b) = pure $ RIR.Expr'Bool ty sp b
convert_expr (HIR.Expr'Tuple ty sp a b) = RIR.Expr'Tuple ty sp <$> convert_expr a <*> convert_expr b
convert_expr (HIR.Expr'Lambda ty sp param_pat body) =
    let param_ty = HIR.pattern_type param_pat
        body_ty = HIR.expr_type body
        body_sp = HIR.expr_span body
    in
    -- '\ (...) -> body' becomes '\ (arg) -> let ... = arg; body'
    new_bound_value param_ty (HIR.pattern_span param_pat) >>= \ param_bk ->
    assign_pattern param_pat (RIR.Expr'Identifier param_ty (HIR.pattern_span param_pat) (Just param_bk)) >>= \ bindings ->
    RIR.Expr'Lambda ty sp param_bk <$> (RIR.Expr'Let body_ty body_sp bindings <$> convert_expr body)

convert_expr (HIR.Expr'Let ty sp bindings body) = RIR.Expr'Let ty sp <$> (concat <$> mapM convert_binding bindings) <*> convert_expr body
convert_expr (HIR.Expr'BinaryOps void _ _ _ _) = absurd void
convert_expr (HIR.Expr'Call ty sp callee arg) = RIR.Expr'Call ty sp <$> convert_expr callee <*> convert_expr arg
convert_expr (HIR.Expr'If ty sp if_sp cond true false) = RIR.Expr'Switch ty sp <$> convert_expr cond <*> sequence [(,) (RIR.Switch'BoolLiteral True) <$> convert_expr true, (,) (RIR.Switch'BoolLiteral False) <$> convert_expr false]
convert_expr (HIR.Expr'Case ty sp case_sp expr arms) = todo -- TODO: case desguaring RIR.Expr'Switch ty sp <$> convert_expr expr <*> mapM (\ (pat, expr) -> (,) <$> convert_pattern pat <*> convert_expr expr) arms
convert_expr (HIR.Expr'Poison ty sp) = pure $ RIR.Expr'Poison ty sp
convert_expr (HIR.Expr'TypeAnnotation _ _ _ other) = convert_expr other

assign_pattern :: HIRPattern -> RIRExpr -> ConvertState [RIRBinding]
assign_pattern (HIR.Pattern'Identifier _ _ bv) expr = pure [RIR.Binding bv expr]
assign_pattern (HIR.Pattern'Wildcard ty _) expr = pure []
assign_pattern (HIR.Pattern'Tuple whole_ty whole_sp a b) expr =
    let a_sp = HIR.pattern_span a
        b_sp = HIR.pattern_span b
        a_ty = HIR.pattern_type a
        b_ty = HIR.pattern_type b
    in
    --     (..., ...) = e
    -- becomes
    --     whole = e
    --     ... = case whole { (a, _) -> a }
    --     ... = case whole { (_, b) -> b }

    new_bound_value whole_ty whole_sp >>= \ whole_bv ->
    new_bound_value a_ty a_sp >>= \ a_bv ->
    new_bound_value b_ty b_sp >>= \ b_bv ->

    let whole_expr = RIR.Expr'Identifier whole_ty whole_sp (Just whole_bv)
        extract_a = RIR.Expr'Switch a_ty a_sp whole_expr [(RIR.Switch'Tuple (Just a_bv) Nothing, RIR.Expr'Identifier a_ty a_sp (Just a_bv))]
        extract_b = RIR.Expr'Switch b_ty b_sp whole_expr [(RIR.Switch'Tuple Nothing (Just b_bv), RIR.Expr'Identifier b_ty b_sp (Just b_bv))]
    in

    assign_pattern a extract_a >>= \ assign_a ->
    assign_pattern b extract_b >>= \ assign_b ->

    pure (RIR.Binding whole_bv expr : assign_a ++ assign_b)

assign_pattern (HIR.Pattern'Named ty sp _ bv other) expr =
    --      a@... = e
    --  becomes
    --      a = e
    --      ... = a
    assign_pattern other (RIR.Expr'Identifier ty sp (Just $ unlocate bv)) >>= \ other_assignments ->
    pure (RIR.Binding (unlocate bv) expr : other_assignments)

assign_pattern (HIR.Pattern'Poison ty _) expr = pure []
