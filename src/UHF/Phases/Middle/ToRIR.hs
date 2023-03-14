module UHF.Phases.Middle.ToRIR (convert) where

import UHF.Util.Prelude

import qualified Arena
import qualified Unique

import UHF.IO.Span (Span)
import UHF.IO.Located (Located (unlocate))

import qualified UHF.Data.IR.HIR as HIR
import qualified UHF.Data.IR.RIR as RIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.ID as ID
import UHF.Data.IR.Keys

import qualified Data.Map as Map

type Type = Maybe (Type.Type Void)

type HIR = HIR.HIR (Located (Maybe BoundValueKey)) Type Type Void
type HIRDecl = HIR.Decl (Located (Maybe BoundValueKey)) Type Type Void
type HIRExpr = HIR.Expr (Located (Maybe BoundValueKey)) Type Type Void
type HIRPattern = HIR.Pattern (Located (Maybe BoundValueKey)) Type
type HIRBinding = HIR.Binding (Located (Maybe BoundValueKey)) Type Type Void

type RIRDecl = RIR.Decl ()
type RIRExpr = RIR.Expr ()
type RIRBinding = RIR.Binding ()

type HIRBoundValueArena = Arena.Arena (HIR.BoundValue Type) BoundValueKey

type ConvertState = Unique.UniqueMakerT (WriterT (Map BoundValueKey RIR.BoundWhere) (State HIRBoundValueArena))

convert :: HIR -> RIR.RIR ()
convert (HIR.HIR decls adts type_synonyms bvs mod) =
    let ((decls', bound_wheres), bvs') = runState (runWriterT (Unique.run_unique_maker_t (Arena.transformM convert_decl decls))) bvs
        bvs'' = Arena.transform_with_key (\ key (HIR.BoundValue id ty sp) -> RIR.BoundValue id ty (bound_wheres Map.! key) sp) bvs'
    in RIR.RIR decls' adts type_synonyms bvs'' mod

convert_decl :: HIRDecl -> ConvertState RIRDecl
convert_decl (HIR.Decl'Module _ _ bindings adts syns) = RIR.Decl'Module <$> (concat <$> mapM (convert_binding RIR.InModule) bindings) <*> pure adts <*> pure syns
convert_decl (HIR.Decl'Type ty) = pure $ RIR.Decl'Type ty

convert_binding :: RIR.BoundWhere -> HIRBinding -> ConvertState [RIRBinding]
convert_binding bound_where (HIR.Binding pat _ expr) = convert_expr bound_where expr >>= assign_pattern bound_where pat

map_bound_where :: BoundValueKey -> RIR.BoundWhere -> ConvertState ()
map_bound_where k w = lift $ tell (Map.singleton k w)

new_bound_value :: ID.BoundValueID -> RIR.BoundWhere -> Type -> Span -> ConvertState BoundValueKey
new_bound_value id bound_where ty sp = lift (lift (state (Arena.put (HIR.BoundValue id ty sp)))) >>= \ key -> map_bound_where key bound_where >> pure key

convert_expr :: RIR.BoundWhere -> HIRExpr -> ConvertState RIRExpr
convert_expr _ (HIR.Expr'Identifier id ty sp bv) = pure $ RIR.Expr'Identifier id ty sp (unlocate bv)
convert_expr _ (HIR.Expr'Char id ty sp c) = pure $ RIR.Expr'Char id ty sp c
convert_expr _ (HIR.Expr'String id ty sp s) = pure $ RIR.Expr'String id ty sp s
convert_expr _ (HIR.Expr'Int id ty sp i) = pure $ RIR.Expr'Int id ty sp i
convert_expr _ (HIR.Expr'Float id ty sp f) = pure $ RIR.Expr'Float id ty sp f
convert_expr _ (HIR.Expr'Bool id ty sp b) = pure $ RIR.Expr'Bool id ty sp b
convert_expr bound_where (HIR.Expr'Tuple id ty sp a b) = RIR.Expr'Tuple id ty sp <$> convert_expr bound_where a <*> convert_expr bound_where b
convert_expr _ (HIR.Expr'Lambda id ty sp param_pat body) =
    let param_ty = HIR.pattern_type param_pat
        body_ty = HIR.expr_type body
        body_sp = HIR.expr_span body
    in
    -- '\ (...) -> body' becomes '\ (arg) -> let ... = arg; body'
    Unique.make_unique >>= \ uniq ->
    new_bound_value (ID.BoundValueID'MadeUpLambdaParam id) (RIR.InLambdaBody uniq) param_ty (HIR.pattern_span param_pat) >>= \ param_bk ->
    assign_pattern (RIR.InLambdaBody uniq) param_pat (RIR.Expr'Identifier id param_ty (HIR.pattern_span param_pat) (Just param_bk)) >>= \ bindings ->
    RIR.Expr'Lambda id ty sp uniq () param_bk <$> (RIR.Expr'Let id body_ty body_sp bindings <$> convert_expr (RIR.InLambdaBody uniq) body)

convert_expr bound_where (HIR.Expr'Let id ty sp bindings body) = RIR.Expr'Let id ty sp <$> (concat <$> mapM (convert_binding bound_where) bindings) <*> convert_expr bound_where body
convert_expr _ (HIR.Expr'BinaryOps _ void _ _ _ _) = absurd void
convert_expr bound_where (HIR.Expr'Call id ty sp callee arg) = RIR.Expr'Call id ty sp <$> convert_expr bound_where callee <*> convert_expr bound_where arg
convert_expr bound_where (HIR.Expr'If id ty sp _ cond true false) = RIR.Expr'Switch id ty sp <$> convert_expr bound_where cond <*> sequence [(,) (RIR.Switch'BoolLiteral True) <$> convert_expr bound_where true, (,) (RIR.Switch'BoolLiteral False) <$> convert_expr bound_where false]
convert_expr _ (HIR.Expr'Case _ _ _ _ _ _) = todo -- TODO: case desguaring RIR.Expr'Switch id ty sp <$> convert_expr expr <*> mapM (\ (pat, expr) -> (,) <$> convert_pattern pat <*> convert_expr expr) arms
convert_expr _ (HIR.Expr'Poison id ty sp) = pure $ RIR.Expr'Poison id ty sp
convert_expr bound_where (HIR.Expr'TypeAnnotation _ _ _ _ other) = convert_expr bound_where other

assign_pattern :: RIR.BoundWhere -> HIRPattern -> RIRExpr -> ConvertState [RIRBinding]
assign_pattern bound_where (HIR.Pattern'Identifier _ _ _ bv) expr = map_bound_where bv bound_where >> pure [RIR.Binding bv expr]
assign_pattern _ (HIR.Pattern'Wildcard _ _ _) _ = pure []
assign_pattern bound_where (HIR.Pattern'Tuple id whole_ty whole_sp a b) expr =
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

    new_bound_value (ID.BoundValueID'MadeUpPat id) bound_where whole_ty whole_sp >>= \ whole_bv ->
    new_bound_value (ID.BoundValueID'MadeUpTupleLeft id) bound_where a_ty a_sp >>= \ a_bv ->
    new_bound_value (ID.BoundValueID'MadeUpTupleRight id) bound_where b_ty b_sp >>= \ b_bv ->

    let whole_expr = RIR.Expr'Identifier todo whole_ty whole_sp (Just whole_bv)
        extract_a = RIR.Expr'Switch todo a_ty a_sp whole_expr [(RIR.Switch'Tuple (Just a_bv) Nothing, RIR.Expr'Identifier todo a_ty a_sp (Just a_bv))]
        extract_b = RIR.Expr'Switch todo b_ty b_sp whole_expr [(RIR.Switch'Tuple Nothing (Just b_bv), RIR.Expr'Identifier todo b_ty b_sp (Just b_bv))]
    in

    assign_pattern bound_where a extract_a >>= \ assign_a ->
    assign_pattern bound_where b extract_b >>= \ assign_b ->

    pure (RIR.Binding whole_bv expr : assign_a ++ assign_b)

assign_pattern bound_where (HIR.Pattern'Named _ ty sp _ bv other) expr =
    --      a@... = e
    --  becomes
    --      a = e
    --      ... = a
    map_bound_where (unlocate bv) bound_where >>
    assign_pattern bound_where other (RIR.Expr'Identifier todo ty sp (Just $ unlocate bv)) >>= \ other_assignments ->
    pure (RIR.Binding (unlocate bv) expr : other_assignments)

assign_pattern _ (HIR.Pattern'Poison _ _ _) _ = pure []
