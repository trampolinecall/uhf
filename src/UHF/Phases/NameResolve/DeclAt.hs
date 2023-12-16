module UHF.Phases.NameResolve.DeclAt (DeclAt (..)) where

-- TODO: figure out a better solution than to split this into this tiny separate module
-- TODO: clean up this and the 3 modules too

import UHF.Prelude

import UHF.Source.Span (Span)

-- TODO: do not export this
data DeclAt = DeclAt Span | ImplicitPrim deriving Show
