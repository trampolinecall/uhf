{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module UHF.Diagnostic.Section
    ( Section(..)
    , SomeSection(..)
    ) where

import UHF.Util.Prelude

import qualified UHF.Diagnostic.Line as Line

data SomeSection = forall a. Section a => SomeSection a

class Section s where
    render :: s -> [Line.Line]
instance Section [Line.Line] where
    render = identity

instance Section SomeSection where
    render (SomeSection s) = render s
