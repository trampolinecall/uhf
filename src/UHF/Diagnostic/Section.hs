{-# LANGUAGE FlexibleInstances #-}

module UHF.Diagnostic.Section
    ( Section(..)
    , ToSection(..)
    , to_section
    ) where

import UHF.Util.Prelude

import qualified UHF.Diagnostic.Line as Line

newtype Section = Section { section_contents :: [Line.Line] }
class ToSection s where
    to_section' :: s -> [Line.Line]
instance ToSection [Line.Line] where
    to_section' = identity

to_section :: ToSection s => s -> Section
to_section = Section . to_section'
