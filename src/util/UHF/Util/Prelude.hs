{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module UHF.Util.Prelude
    ( module X

    , Prelude.seq

    , identity

    , undefined
    , error
    , trace
    , trace_show_id
    , trace_with_message
    , not_implemented

    , putStr
    , putStrLn
    , hPutStr
    , hPutStrLn
    , putText
    , hPutText

    , convert_str

    , show

    , FormattedString(..)
    , Format(..)
    ) where

-- a lot of this is based on the stephen diehl's protolude (https://www.stephendiehl.com/posts/protolude.html)

import qualified Prelude
import qualified Data.Function (id)
import qualified Data.Text
import qualified Data.Text.IO
import qualified Debug.Trace
import qualified System.IO
import qualified UHF.FormattedString
import Control.Monad.IO.Class (MonadIO, liftIO)

import GHC.IO as X (IO)
import GHC.Num as X (Num(..), Integer)
import GHC.Real as X (Rational, Real(..), Integral(..), Fractional(..), RealFrac(..), odd, even, fromIntegral, (^), (^^))
import GHC.Float as X (Float, Double)
import GHC.Show as X (Show)

import Data.Int as X (Int)
import Data.Bits as X (Bits(..))
import Data.Bool as X (Bool(..), otherwise, (&&), (||), not)
import Data.Char as X (Char)
import Data.Maybe as X (Maybe(..), maybe, isJust, isNothing, fromMaybe, listToMaybe, maybeToList, catMaybes, mapMaybe)
import Data.Either as X (Either(..), either, isLeft, isRight)
import Data.Function as X (const, (.), flip, ($), (&), on)

import Data.Tuple as X
import Data.Void as X (Void, absurd)
import Data.List as X ((++), head, last, init, tail, uncons, map, reverse, intersperse, intercalate, iterate, iterate', repeat, replicate, take, drop, splitAt, takeWhile, dropWhile, span, break, lookup, filter, zip, zip3, zipWith, zipWith3) -- TODO: remove head, last, init, tail, lookup, zip, zip3, zipWith, zipWith3, also probabyl just clean it up anyways

import Data.Functor as X (Functor, fmap, (<$>), (<&>))
import Data.Eq as X
import Data.Ord as X
import Data.Semigroup as X (Semigroup, (<>))
import Data.Monoid as X (Monoid, mempty)
import Data.Foldable as X hiding (foldr1, foldl1, maximumBy, minimum, minimumBy) -- TODO: hide maximum, minimum because they are partial
import Data.Traversable as X (Traversable(..), for, forM, mapAccumL, mapAccumR)

import Safe as X (headMay, tailMay, initMay, lastMay, headDef, tailDef, initDef, lastDef)

import Data.List.NonEmpty as X (NonEmpty(..))
import Data.Map as X (Map)
import Data.Set as X (Set)
import Data.Sequence as X (Seq)
import Data.IntMap as X (IntMap)
import Data.IntSet as X (IntSet)

import Data.Text as X (Text)

import Control.Monad.Trans.State as X (State, state, runState, evalState, execState, StateT(..), evalStateT, execStateT, get, put, modify, modify')
import Control.Monad.Trans.Writer as X (Writer, writer, runWriter, execWriter, mapWriter, WriterT(..), execWriterT, mapWriterT, tell)
import Control.Monad.Trans.Reader as X (Reader, reader, runReader, ReaderT(..), ask)
import Control.Monad.Trans.Except as X (Except, except, runExcept, ExceptT(..), runExceptT)
import Control.Monad.Trans.Class as X (lift)

import Control.Applicative as X (Applicative, pure, (<*>))
import Control.Monad as X (Monad, (>>=), (>>))

import System.Exit as X (exitFailure, exitSuccess)
import System.Environment as X (getArgs, getProgName)
import System.IO as X (FilePath, Handle, stdin, stdout, stderr)

import UHF.FormattedString as X (FormattedString)

import Test.Tasty as X (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit as X (Assertion, testCase, (@=?), (@?=), assertBool, assertFailure)
import Test.Tasty.TH as X (testGroupGenerator)

-- these are also mostly taken from the protolude, especially the Print class and the ConvertString class

identity :: a -> a
identity = Data.Function.id

{-# WARNING undefined "'undefined'" #-}
undefined :: a
undefined = Prelude.undefined

{-# WARNING error "'error'" #-}
error :: Prelude.String -> a
error = Prelude.error

{-# WARNING trace "'trace'" #-}
trace :: Prelude.String -> a -> a
trace = Debug.Trace.trace

{-# WARNING trace_show_id "'trace_show_id'" #-}
trace_show_id :: Show a => a -> a
trace_show_id = Debug.Trace.traceShowId

{-# WARNING trace_with_message "'trace_show_id'" #-}
trace_with_message :: Show a => Prelude.String -> a -> a
trace_with_message msg a = Debug.Trace.trace (msg ++ ": " ++ show a) a

{-# WARNING not_implemented "'not_implemented'" #-}
not_implemented :: a
not_implemented = Prelude.error "not implemented yet"

class Print a where
  hPutStr :: MonadIO m => Handle -> a -> m ()
  hPutStrLn :: MonadIO m => Handle -> a -> m ()

instance Print Data.Text.Text where
  hPutStr h = liftIO . Data.Text.IO.hPutStr h
  hPutStrLn h = liftIO . Data.Text.IO.hPutStrLn h

instance Print [Char] where
  hPutStr h = liftIO . System.IO.hPutStr h
  hPutStrLn h = liftIO . System.IO.hPutStrLn h

putStr :: (Print a, MonadIO m) => a -> m ()
putStr = hPutStr stdout

putStrLn :: (Print a, MonadIO m) => a -> m ()
putStrLn = hPutStrLn stdout

putText :: MonadIO m => Data.Text.Text -> m ()
putText = putStr
{-# SPECIALIZE putText :: Data.Text.Text -> IO () #-}

hPutText :: MonadIO m => Handle -> Data.Text.Text -> m ()
hPutText = hPutStr
{-# SPECIALIZE putText :: Data.Text.Text -> IO () #-}

show :: (Show a, ConvertString Prelude.String b) => a -> b
show = convert_str . Prelude.show

class ConvertString a b where
  convert_str :: a -> b

instance ConvertString Prelude.String Prelude.String where convert_str = identity
instance ConvertString Prelude.String Data.Text.Text where convert_str = Data.Text.pack
instance ConvertString Prelude.String FormattedString where convert_str = UHF.FormattedString.Literal . Data.Text.pack

instance ConvertString Data.Text.Text Prelude.String where convert_str = Data.Text.unpack
instance ConvertString Data.Text.Text Data.Text.Text where convert_str = identity
instance ConvertString Data.Text.Text FormattedString where convert_str = UHF.FormattedString.Literal

-- cannot convert to other 2 string types without losing sgr information
instance ConvertString FormattedString FormattedString where convert_str = identity

class Format a where
    format :: a -> FormattedString
