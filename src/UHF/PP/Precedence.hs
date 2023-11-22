-- utilities for dealing with precedence in pretty printers
module UHF.PP.Precedence
    ( PPGivenCurrentAndNextLevels
    , Levels
    , pp_precedence

    , tests
    ) where

import UHF.Util.Prelude

import qualified UHF.PP as PP

type PPGivenCurrentAndNextLevels thing = (thing -> PP.Token) -> (thing -> PP.Token) -> PP.Token
type Levels thing = thing -> (Int, PPGivenCurrentAndNextLevels thing)

pp_precedence :: Levels thing -> thing -> PP.Token
pp_precedence levels = helper 0
    where
        helper current_precedence thing =
            let (level, pp) = levels thing
            in if level >= current_precedence
                then pp (helper level) (helper (level + 1))
                else PP.List ["(", pp (helper 0) (helper 1), ")"]

-- tests {{{1
data AdditionAndMultiplication
    = Primary Text
    | Addition AdditionAndMultiplication AdditionAndMultiplication
    | Multiplication AdditionAndMultiplication AdditionAndMultiplication
test_addition_and_multiplication :: [TestTree]
test_addition_and_multiplication =
    let pp = pp_precedence
            (\case
                Addition a b -> (0, \ cur next -> PP.List [cur a, " + ", next b])
                Multiplication a b -> (1, \ cur next -> PP.List [cur a, " * ", next b])
                Primary s -> (2, \ _ _ -> PP.String s)
            )

        make_test_case name expr expect =
            testCase name $
                let pped = PP.render $ pp expr
                in expect @=? pped

        a = Primary "a"
        b = Primary "b"
        c = Primary "c"

        add = Addition
        mul = Multiplication
    in
        [ make_test_case "simple primary" a "a"
        , make_test_case "simple addition" (a `add` b) "a + b"
        , make_test_case "simple multiplication" (a `mul` b) "a * b"

        , make_test_case "mixed, no parentheses" ((a `mul` b) `add` c) "a * b + c"
        , make_test_case "mixed, with parentheses" ((a `add` b) `mul` c) "(a + b) * c"

        , make_test_case "+ assoc, no parentheses" ((a `add` b) `add` c) "a + b + c"
        , make_test_case "+ assoc, needs parentheses" (a `add` (b `add` c)) "a + (b + c)"

        , make_test_case "* assoc, no parentheses" ((a `mul` b) `mul` c) "a * b * c"
        , make_test_case "* assoc, needs parentheses" (a `mul` (b `mul` c)) "a * (b * c)"
        ]

tests :: TestTree
tests = $(testGroupGenerator)
