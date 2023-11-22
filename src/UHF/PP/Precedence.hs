-- utilities for dealing with precedence in pretty printers
module UHF.PP.Precedence
    ( PPGivenCurrentAndNextLevels
    , Levels
    , pp_precedence
    ) where

import UHF.Util.Prelude

import qualified UHF.PP as PP

type PPGivenCurrentAndNextLevels thing = (thing -> PP.Token) -> (thing -> PP.Token) -> thing -> PP.Token
type Levels thing = thing -> (Int, PPGivenCurrentAndNextLevels thing)

pp_precedence :: Levels thing -> thing -> PP.Token
pp_precedence levels = helper 0
    where
        helper current_precedence thing =
            let (level, pp) = levels thing
                pped = pp (helper current_precedence) (helper (current_precedence + 1)) thing
            in if level >= current_precedence
                then pped
                else PP.List ["(", pped, ")"]
