{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module UHF.Parts.UnifiedFrontendSolver.InfixGroup.Prepare (prepare) where

import UHF.Prelude

import qualified UHF.Data.IR.TypeWithInferVar as TypeWithInferVar
import qualified UHF.Data.SIR as SIR
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.Misc.Result (InfixGroupedArena, InfixGroupedKey)
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.Task (InfixGroupTask (..))
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NameMaps as NameMaps
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result (IdenResolvedKey, TypeExprEvaledAsTypeKey, TypeExprEvaledKey)
import UHF.Parts.UnifiedFrontendSolver.SolveResult
import qualified UHF.Util.Arena as Arena
import Data.Data (Proxy (..))
import UHF.Data.SIR.Visitor
import qualified UHF.Data.IR.Type.ADT as Type.ADT

type Unprepared = (NameMaps.NameContextKey, IdenResolvedKey (), TypeWithInferVar.Type, TypeExprEvaledKey, TypeExprEvaledAsTypeKey, (), ())
type Prepared = (NameMaps.NameContextKey, IdenResolvedKey (), TypeWithInferVar.Type, TypeExprEvaledKey, TypeExprEvaledAsTypeKey, (), InfixGroupedKey)

type PrepareState = WriterT [InfixGroupTask] (State InfixGroupedArena)

new_infix_grouped_key :: (InfixGroupedKey -> InfixGroupTask) -> PrepareState InfixGroupedKey
new_infix_grouped_key task = do
    key <- state $ \arena -> let (key, arena') = Arena.put (Inconclusive ()) arena in (key, arena')
    tell [task key]
    pure key

data PrepareVisitor

prepare :: SIR.SIR Unprepared -> (SIR.SIR Prepared, InfixGroupedArena, [InfixGroupTask])
prepare sir =
    let ((sir', tasks), arenas) = runState (runWriterT (visit_sir' (Proxy :: Proxy PrepareVisitor) sir)) Arena.new
    in (sir', arenas, tasks)

instance TransformsNameMapIndex Unprepared Prepared () PrepareState PrepareVisitor where
instance TransformsIdenResolvedKey Unprepared Prepared (SIR.DeclRef TypeWithInferVar.Type) (SIR.DeclRef TypeWithInferVar.Type) () PrepareState PrepareVisitor where
instance TransformsIdenResolvedKey Unprepared Prepared SIR.ValueRef SIR.ValueRef () PrepareState PrepareVisitor where
instance TransformsIdenResolvedKey Unprepared Prepared Type.ADT.VariantIndex Type.ADT.VariantIndex () PrepareState PrepareVisitor where
instance TransformsTypeExprEvaledKey Unprepared Prepared () PrepareState PrepareVisitor where
instance TransformsTypeExprEvaledAsTypeKey Unprepared Prepared () PrepareState PrepareVisitor where
instance TransformsTypeInfo Unprepared Prepared () PrepareState PrepareVisitor where

instance SIRVisitor Unprepared Prepared () PrepareState () PrepareVisitor where
instance CUVisitor Unprepared Prepared () PrepareState () PrepareVisitor where
instance ADTVisitor Unprepared Prepared () PrepareState () PrepareVisitor where
instance TypeSynonymVisitor Unprepared Prepared () PrepareState () PrepareVisitor where
instance ModuleVisitor Unprepared Prepared () PrepareState () PrepareVisitor where
instance VariableVisitor Unprepared Prepared () PrepareState () PrepareVisitor where
instance BindingVisitor Unprepared Prepared () PrepareState () PrepareVisitor where
instance TypeExprVisitor Unprepared Prepared () PrepareState () PrepareVisitor where

instance ExprIdentifierRefVisitor Unprepared Prepared () PrepareState () PrepareVisitor where
instance OperatorRefVisitor Unprepared Prepared () PrepareState () PrepareVisitor where
instance ExprVisitor Unprepared Prepared () PrepareState () PrepareVisitor where
    visit_expr_binary_ops proxy () id () type_info sp first ops = do
        first <- visit_expr' proxy first
        ops <- mapM (\(sp, iden, rhs) -> (sp,,) <$> visit_operator_ref' proxy iden <*> visit_expr' proxy rhs) ops

        infix_group_key <- new_infix_grouped_key $ InfixGroupTask (map (\(_, iden, _) -> SIR.split_identifier_resolved iden) ops)

        pure (SIR.Expr'BinaryOps id infix_group_key type_info sp first ops, ())

instance PatternADTVariantRefVisitor Unprepared Prepared () PrepareState () PrepareVisitor where
instance PatternVisitor Unprepared Prepared () PrepareState () PrepareVisitor where
