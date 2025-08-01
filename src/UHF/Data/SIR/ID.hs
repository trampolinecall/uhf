{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UHF.Data.SIR.ID
    ( ID
    , GenT
    , Gen
    , run_t
    , run
    , gen_id
    ) where

import UHF.Prelude

import qualified Control.Monad.Fix as Fix
import qualified Data.Functor.Identity as Identity
import qualified Data.Map as Map
import Data.Proxy (Proxy (..))
import Data.String (String)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- allow ids to be parameterized by type level strings so that we can easily create new ids to serve as keys for maps and enforce a little bit more
-- type safety. for example, we can attach an (ID "Expr") field to every variant of an expression so that type checking can store its results in a
-- (Map (ID "Expr") Type), and if we make sure to walk the entire SIR, every possible (ID "Expr") from the SIR is guaranteed to have an entry in that
-- map. other operations that only attach results to certain variants, such as name resolution that only attaches information to Expr'Refer and a few
-- other nodes, can create (ID "ReferExpr") or similar ids for other things and store its results in a (Map (ID "ReferExpr") ValueRef) so that we can
-- be a little more sure that every refer expr has a result and to prevent us from trying to get the name resolution result of some other kind of
-- expression, like a call expression or something.
newtype ID (thing :: Symbol) = ID Int deriving (Show, Eq, Ord)
newtype GenT m r = GenT (StateT (Map String Int) m r) deriving (Functor, Applicative, Monad, MonadTrans, Fix.MonadFix)
type Gen = GenT Identity.Identity

run_t :: Monad m => GenT m r -> m r
run_t (GenT s) = evalStateT s Map.empty
run :: Gen r -> r
run = Identity.runIdentity . run_t

gen_id :: forall (thing :: Symbol) m. (KnownSymbol thing, Monad m) => GenT m (ID thing)
gen_id = GenT $ StateT $ \is -> do
    let thing_name = symbolVal (Proxy :: Proxy thing)
    let current_highest = Map.findWithDefault 0 thing_name is
    let new_id = ID current_highest
    let is' = Map.insert thing_name (current_highest + 1) is

    pure (new_id, is')
