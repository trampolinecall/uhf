{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UHF.Parts.SolveTypes.Error.InferVarNamer (InferVarNamer, run_infer_var_namer, name_infer_var) where

import UHF.Prelude

import Data.Char (isAsciiUpper)
import qualified Data.Map as Map
import qualified Data.Text as Text
import GHC.Enum (Enum (succ))

import UHF.Parts.SolveTypes.Aliases
import UHF.Source.Located (Located (..))
import UHF.Source.Span (Span)
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.PP as Type.PP
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.PP as PP
import qualified UHF.Parts.TypeSolver as TypeSolver
import qualified UHF.Util.Arena as Arena

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

       there are is a special case: when there is only 1 digit and that digit is 'Z', replace it with 'AA'
       recursively incrementing will try to increment '', which is a case i will choose to ignore (it is possible to get the same sequence by making this result in 'A', however)

       the case where there are multiple digits and they are all 'Z' is handled by the above logic because the above logic will recursively call increment on smaller sections of 'Z's until there is only 1 'Z' left, at which point it becomes 'AA'
       for example, incrementing 'ZZZ' will try to increment 'ZZ', which will try to increment 'Z', which in turn becomes 'AA'
-}
inc_name :: InferVarName -> InferVarName
inc_name (InferVarName n) = InferVarName (inc n)
    where
        inc ('Z':[]) = "AA"
        inc ('Z':rest) = 'A' : inc rest
        inc (first:rest) = inc_char first : rest
        inc [] = error "inc called with empty name"

        inc_char :: Char -> Char
        inc_char 'Z' = error "inc_char called with 'Z'"
        inc_char ch | isAsciiUpper ch = succ ch
        inc_char ch = error $ "inc_char should only be called with uppercase ascii letters; got " ++ show ch
