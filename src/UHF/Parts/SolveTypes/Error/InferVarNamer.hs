{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UHF.Parts.SolveTypes.Error.InferVarNamer (InferVarNamer) where

import UHF.Prelude

import qualified Data.Map as Map

import UHF.Parts.SolveTypes.Aliases
import qualified UHF.Parts.TypeSolver as TypeSolver
import UHF.Source.Located (Located (..))
import UHF.Source.Span (Span)
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.PP as Type.PP
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.PP as PP
import qualified UHF.Util.Arena as Arena

newtype InferVarNamer a = InferVarNamer (State (Int, Map TypeSolver.InferVarKey Text) a) deriving (Functor, Applicative, Monad)

-- TODO: adjust these depending on what names are in scope

run_infer_var_namer :: InferVarNamer a -> (a, Map TypeSolver.InferVarKey Text)
run_infer_var_namer (InferVarNamer s) = let (r, (_, n)) = runState s (1, Map.empty) in (r, n)

name_infer_var :: TypeSolver.InferVarKey -> InferVarNamer Text
name_infer_var var = InferVarNamer $ state $
    \ (cur_n, cache) ->
        case Map.lookup var cache of
            Just res -> (res, (cur_n, cache))
            Nothing ->
                let name = make_name_from_n cur_n
                in (name, (cur_n + 1, Map.insert var name cache))

make_name_from_n :: Int -> Text
make_name_from_n n = todo

{-
    for this example, let's reduce the amount of digits from 26 to 3
    so the sequence goes
     1   A ┐
     2   B │ this section is 3^1 = 3 long
     3   C ┘
     4  AA ┐
     5  AB │ this section is 3^2 = 9 long
     6  AC │
     7  BA │
     8  BB │
     9  BC │
    10  CA │
    11  CB │
    12  CC ┘
    13 AAA ┐
    14 AAB │ this section is 3^3 = 27 long
    15 AAC │
    16 ABA │
    17 ABB │
    18 ABC │
    19 ACA │
    20 ACB │
    21 ACC │
    22 BAA │
    23 BAB │
    24 BAC │
    25 BBA │
    26 BBB │
    27 BBC │
    28 BCA │
    29 BCB │
    30 BCC │
    31 CAA │
    32 CAB │
    33 CAC │
    34 CBA │
    35 CBB │
    36 CBC │
    37 CCA │
    38 CCB │
    39 CCC ┘
     .   . ┐
     .   . │ this section is 3^4 = 81 long
     .   . │
-}

                {- TODO: make this work correctly
                -- goes 'A', 'B', 'C', ..., 'Y', 'Z', 'AA', 'AB', 'AC', ..., 'AY', 'AZ', 'BA', 'BB', 'BC', ...
                let name = Text.pack $ map to_characters $ reverse $ make_name [] cur_n
                in (name, (cur_n + 1, Map.insert var name cache))
    where
        make_name acc n
            | n > 0 =
                make_name ((n `mod` 26) : acc) (n `div` 26)
            | otherwise = acc

        to_characters n = chr $ 65 + n
    -}
