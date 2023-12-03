module UHF.Data.RIR
    ( RIR (..)
    , CU (..)

    , Binding (..)

    , VariableKey
    , Variable (..)

    , Type
    , Expr (..)
    , MatchTree (..)
    , MatchClause (..)
    , MatchMatcher (..)
    , MatchAssignRHS (..)
    , expr_type
    , expr_span
    ) where

import UHF.Prelude

import UHF.Data.IR.Keys
import UHF.Source.Span (Span)
import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Util.Arena as Arena

-- "reduced ir"
-- not used a lot; serves mostly as a intermediary step where a lot of things get desugared to make the transition to anfir easier
data RIR
    = RIR
        (Arena.Arena (Type.ADT (Maybe (Type.Type Void))) ADTKey)
        (Arena.Arena (Type.TypeSynonym (Maybe (Type.Type Void))) TypeSynonymKey)
        (Arena.Arena Type.QuantVar Type.QuantVarKey)
        (Arena.Arena Variable VariableKey)
        CU

data Variable
    = Variable
        { var_id :: ID.VariableID
        , var_ty :: Maybe (Type.Type Void)
        , var_sp :: Span
        }
    deriving Show

-- "compilation unit"
data CU = CU [Binding] [ADTKey] [TypeSynonymKey]

data Binding = Binding VariableKey Expr deriving Show

type Type = Type.Type Void

data Expr
    -- identifiers need explicit types in case there the identifiers form loops
    = Expr'Identifier ID.ExprID (Maybe Type) Span (Maybe VariableKey)
    | Expr'Char ID.ExprID Span Char
    | Expr'String ID.ExprID Span Text
    | Expr'Int ID.ExprID Span Integer
    | Expr'Float ID.ExprID Span Rational
    | Expr'Bool ID.ExprID Span Bool -- TODO: replace with identifier exprs

    | Expr'Tuple ID.ExprID Span Expr Expr

    | Expr'Lambda ID.ExprID Span VariableKey Expr

    | Expr'Let ID.ExprID Span [Binding] Expr

    | Expr'Call ID.ExprID Span Expr Expr

    -- match needs to know its type in case its match tree has no arms
    | Expr'Match ID.ExprID (Maybe Type) Span MatchTree

    | Expr'Forall ID.ExprID Span (NonEmpty QuantVarKey) Expr
    | Expr'TypeApply ID.ExprID (Maybe Type) Span Expr (Maybe Type) -- TODO: remove type from this

    | Expr'MakeADT ID.ExprID Span Type.ADT.VariantIndex [Maybe Type] [Expr]

    | Expr'Poison ID.ExprID (Maybe Type) Span
    deriving Show

-- TODO: split match things into separate module?
data MatchTree
    = MatchTree [([MatchClause], Either MatchTree Expr)]
    deriving Show
data MatchClause
    = MatchClause'Match VariableKey MatchMatcher
    | MatchClause'Assign VariableKey MatchAssignRHS
    -- eventually bool predicates will be added here
    deriving Show
data MatchMatcher
    = Match'BoolLiteral Bool
    | Match'Tuple
    | Match'AnonADTVariant (Maybe Type.ADT.VariantIndex)
    deriving Show
data MatchAssignRHS
    = MatchAssignRHS'OtherVar VariableKey
    | MatchAssignRHS'TupleDestructure1 (Maybe Type) VariableKey
    | MatchAssignRHS'TupleDestructure2 (Maybe Type) VariableKey
    | MatchAssignRHS'AnonADTVariantField (Maybe Type) VariableKey (Maybe Type.ADT.FieldIndex)
    deriving Show

expr_type :: Arena.Arena Variable VariableKey -> Expr -> Maybe Type
expr_type _ (Expr'Identifier _ ty _ _) = ty
expr_type _ (Expr'Char _ _ _) = Just Type.Type'Char
expr_type _ (Expr'String _ _ _) = Just Type.Type'String
expr_type _ (Expr'Int _ _ _) = Just Type.Type'Int
expr_type _ (Expr'Float _ _ _) = Just Type.Type'Float
expr_type _ (Expr'Bool _ _ _) = Just Type.Type'Bool
expr_type var_arena (Expr'Tuple _ _ a b) = Type.Type'Tuple <$> expr_type var_arena a <*> expr_type var_arena b
expr_type var_arena (Expr'Lambda _ _ param_var body) = Type.Type'Function <$> var_ty (Arena.get var_arena param_var) <*> expr_type var_arena body
expr_type var_arena (Expr'Let _ _ _ res) = expr_type var_arena res
expr_type var_arena (Expr'Call _ _ callee _) =
    let callee_ty = expr_type var_arena callee
    in callee_ty <&> \case
        Type.Type'Function _ res -> res
        _ -> error $ "rir call expression created with callee of type " <> show callee_ty
expr_type _ (Expr'Match _ ty _ _) = ty
expr_type var_arena (Expr'Forall _ _ tyvars res) = Type.Type'Forall tyvars <$> expr_type var_arena res
expr_type _ (Expr'TypeApply _ ty _ _ _) = ty
expr_type _ (Expr'MakeADT _ _ (Type.ADT.VariantIndex adt_key _) vars _) = Type.Type'ADT adt_key <$> sequence vars
expr_type _ (Expr'Poison _ ty _) = ty

expr_span :: Expr -> Span
expr_span (Expr'Identifier _ _ sp _) = sp
expr_span (Expr'Char _ sp _) = sp
expr_span (Expr'String _ sp _) = sp
expr_span (Expr'Int _ sp _) = sp
expr_span (Expr'Float _ sp _) = sp
expr_span (Expr'Bool _ sp _) = sp
expr_span (Expr'Tuple _ sp _ _) = sp
expr_span (Expr'Lambda _ sp _ _) = sp
expr_span (Expr'Let _ sp _ _) = sp
expr_span (Expr'Call _ sp _ _) = sp
expr_span (Expr'Match _ _ sp _) = sp
expr_span (Expr'Forall _ sp _ _) = sp
expr_span (Expr'TypeApply _ _ sp _ _) = sp
expr_span (Expr'MakeADT _ sp _ _ _) = sp
expr_span (Expr'Poison _ _ sp) = sp
