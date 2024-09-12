{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module UHF.Parts.SolveTypes.Error.InferVarNamer (InferVarNamer, run_infer_var_namer, name_infer_var, tests) where

import UHF.Prelude

import Data.Char (isAsciiUpper)
import qualified Data.Map as Map
import qualified Data.Text as Text
import GHC.Enum (Enum (succ))

import qualified UHF.Parts.TypeSolver as TypeSolver

-- InferVarName's list is in reverse order to the order that the names are displayed
-- this is because the logic in inc_name splits a name into the rightmost digit and the rest of the digits, which matches the structure of a linked list in reverse
-- therefore, the rightmost digit is placed at the beginning of the list
newtype InferVarName = InferVarName [Char]
newtype InferVarNamer a = InferVarNamer (State (InferVarName, Map TypeSolver.InferVarKey Text) a) deriving (Functor, Applicative, Monad)

-- TODO: adjust these depending on what names are in scope

run_infer_var_namer :: InferVarNamer a -> (a, Map TypeSolver.InferVarKey Text)
run_infer_var_namer (InferVarNamer s) = let (r, (_, n)) = runState s (InferVarName "A", Map.empty) in (r, n)

name_infer_var :: TypeSolver.InferVarKey -> InferVarNamer Text
name_infer_var var = InferVarNamer $ state $
    \ (cur_n, cache) ->
        case Map.lookup var cache of
            Just res -> (res, (cur_n, cache))
            Nothing ->
                let name = show_infer_var_name cur_n
                in (name, (inc_name cur_n, Map.insert var name cache))

show_infer_var_name :: InferVarName -> Text
show_infer_var_name (InferVarName n) = Text.reverse $ Text.pack n

{-
    the sequence of the variables names goes
    A
    B
    C
    ...
    Y
    Z
    AA
    AB
    AC
    ...
    AY
    AZ
    BA
    BB
    BC
    ...
    ZY
    ZZ
    AAA
    ...

    this functions contains the core of the logic implementing this pattern
    when a name is incremented, there are two possibilities:

    1. increment last digit

       if the last digit is not 'Z', increment it to get the new last digit
       for example, given the name 'DGA', increment the last digit and get 'DGB'

    2. roll over increment

       if the last digit is 'Z', 'increment' it to 'A' and recursively roll over to the next places
       for example, given the name 'GBZ', change 'Z' to 'A' and recursively call the increment function on 'GB' to get 'GCA'

       there are is a special case: incrementing an empty string results in 'A'

       the case where there are multiple digits and they are all 'Z' is handled by the above logic because the above logic will recursively call increment on smaller sections of 'Z's until it tries to increment '', at which point it becomes 'A'
       for example, incrementing 'ZZ' will try to increment 'Z', which will try to increment '', which in turn becomes 'A'
-}
inc_name :: InferVarName -> InferVarName
inc_name (InferVarName n) = InferVarName (inc n)
    where
        inc ('Z':rest) = 'A' : inc rest
        inc (first:rest) = inc_char first : rest
        inc [] = ['A']

        inc_char :: Char -> Char
        inc_char 'Z' = error "inc_char called with 'Z'"
        inc_char ch | isAsciiUpper ch = succ ch
        inc_char ch = error $ "inc_char should only be called with uppercase ascii letters; got " ++ show ch

testcase :: [Char] -> [Char] -> Assertion
testcase input expected =
    let InferVarName result = inc_name (InferVarName input)
    in expected @=? result

case_normal_a, case_normal_b, case_z, case_normal_2_a, case_normal_2_b, case_rollover_a, case_rollover_b, case_rollover_z, case_all_zs :: Assertion
case_normal_a = testcase "A" "B"
case_normal_b = testcase "B" "C"
case_z = testcase "Z" "AA"
case_normal_2_a = testcase "AA" "BA"
case_normal_2_b = testcase "BA" "CA"
case_rollover_a = testcase "ZA" "AB"
case_rollover_b = testcase "ZB" "AC"
case_rollover_z = testcase "AZ" "BZ"
case_all_zs = testcase "ZZ" "AAA"

tests :: TestTree
tests = $(testGroupGenerator)
