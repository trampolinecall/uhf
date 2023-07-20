module UHF.Data.IR.RIR
    ( RIR (..)
    , CU (..)

    , Binding (..)

    , BoundValueKey
    , BoundValue (..)

    , Type
    , Expr (..)
    , CaseTree (..)
    , CaseClause (..)
    , CaseMatcher (..)
    , CaseAssignRHS (..)
    , expr_type
    , expr_span
    ) where

import UHF.Util.Prelude

import qualified Arena

import UHF.Data.IR.Keys
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.ID as ID

import UHF.IO.Span (Span)

-- "reduced ir"
-- not used a lot; serves mostly as a intermediary step where a lot of things get desugared to make the transition to anfir easier
data RIR
    = RIR
        (Arena.Arena (Type.ADT (Maybe (Type.Type Void))) ADTKey)
        (Arena.Arena (Type.TypeSynonym (Maybe (Type.Type Void))) TypeSynonymKey)
        (Arena.Arena Type.Var Type.TypeVarKey)
        (Arena.Arena BoundValue BoundValueKey)
        CU

data BoundValue
    = BoundValue
        { bv_id :: ID.BoundValueID
        , bv_ty :: Maybe (Type.Type Void)
        , bv_sp :: Span
        }
    deriving Show

-- "compilation unit"
data CU = CU [Binding] [ADTKey] [TypeSynonymKey]

data Binding = Binding BoundValueKey Expr deriving Show

type Type = Type.Type Void

data Expr
    = Expr'Identifier ID.ExprID Span (Maybe BoundValueKey)
    | Expr'Char ID.ExprID Span Char
    | Expr'String ID.ExprID Span Text
    | Expr'Int ID.ExprID Span Integer
    | Expr'Float ID.ExprID Span Rational
    | Expr'Bool ID.ExprID Span Bool -- TODO: replace with identifier exprs

    | Expr'Tuple ID.ExprID Span Expr Expr

    | Expr'Lambda ID.ExprID Span BoundValueKey Expr

    | Expr'Let ID.ExprID Span [Binding] Expr

    | Expr'Call ID.ExprID Span Expr Expr

    -- case needs to know its type in case its case tree has no arms
    | Expr'Case ID.ExprID (Maybe Type) Span CaseTree

    | Expr'Forall ID.ExprID Span (NonEmpty TypeVarKey) Expr
    | Expr'TypeApply ID.ExprID (Maybe Type) Span Expr (Maybe Type) -- TODO: remove type from this

    | Expr'MakeADT ID.ExprID Span Type.ADTVariantIndex [Maybe Type] [Expr]

    | Expr'Poison ID.ExprID (Maybe Type) Span
    deriving Show

-- TODO: split case things into separate module?
data CaseTree
    = CaseTree [([CaseClause], Either CaseTree Expr)]
    deriving Show
data CaseClause
    = CaseClause'Match BoundValueKey CaseMatcher
    | CaseClause'Assign BoundValueKey CaseAssignRHS
    -- eventually bool predicates will be added here
    deriving Show
data CaseMatcher
    = Case'BoolLiteral Bool
    | Case'Tuple
    | Case'AnonADTVariant (Maybe Type.ADTVariantIndex)
    deriving Show
data CaseAssignRHS
    = CaseAssignRHS'OtherBVK BoundValueKey
    | CaseAssignRHS'TupleDestructure1 (Maybe Type) BoundValueKey
    | CaseAssignRHS'TupleDestructure2 (Maybe Type) BoundValueKey
    | CaseAssignRHS'AnonADTVariantField (Maybe Type) BoundValueKey (Maybe Type.ADTFieldIndex)
    deriving Show

expr_type :: Arena.Arena BoundValue BoundValueKey -> Expr -> Maybe Type
expr_type bv_arena (Expr'Identifier _ _ bv) = Arena.get bv_arena <$> bv >>= bv_ty
expr_type _ (Expr'Char _ _ _) = Just Type.Type'Char
expr_type _ (Expr'String _ _ _) = Just Type.Type'String
expr_type _ (Expr'Int _ _ _) = Just Type.Type'Int
expr_type _ (Expr'Float _ _ _) = Just Type.Type'Float
expr_type _ (Expr'Bool _ _ _) = Just Type.Type'Bool
expr_type bv_arena (Expr'Tuple _ _ a b) = Type.Type'Tuple <$> expr_type bv_arena a <*> expr_type bv_arena b
expr_type bv_arena (Expr'Lambda _ _ param_bv body) = Type.Type'Function <$> bv_ty (Arena.get bv_arena param_bv) <*> expr_type bv_arena body
expr_type bv_arena (Expr'Let _ _ _ res) = expr_type bv_arena res
expr_type bv_arena (Expr'Call _ _ callee _) =
    let callee_ty = expr_type bv_arena callee
    in callee_ty <&> \case
        Type.Type'Function _ res -> res
        _ -> error $ "rir call expression created with callee of type " <> show callee_ty
expr_type _ (Expr'Case _ ty _ _) = ty
expr_type bv_arena (Expr'Forall _ _ tyvars res) = Type.Type'Forall tyvars <$> expr_type bv_arena res
expr_type _ (Expr'TypeApply _ ty _ _ _) = ty
expr_type _ (Expr'MakeADT _ _ (Type.ADTVariantIndex adt_key _) vars _) = Type.Type'ADT adt_key <$> sequence vars
expr_type _ (Expr'Poison _ ty _) = ty

expr_span :: Expr -> Span
expr_span (Expr'Identifier _ sp _) = sp
expr_span (Expr'Char _ sp _) = sp
expr_span (Expr'String _ sp _) = sp
expr_span (Expr'Int _ sp _) = sp
expr_span (Expr'Float _ sp _) = sp
expr_span (Expr'Bool _ sp _) = sp
expr_span (Expr'Tuple _ sp _ _) = sp
expr_span (Expr'Lambda _ sp _ _) = sp
expr_span (Expr'Let _ sp _ _) = sp
expr_span (Expr'Call _ sp _ _) = sp
expr_span (Expr'Case _ _ sp _) = sp
expr_span (Expr'Forall _ sp _ _) = sp
expr_span (Expr'TypeApply _ _ sp _ _) = sp
expr_span (Expr'MakeADT _ sp _ _ _) = sp
expr_span (Expr'Poison _ _ sp) = sp
