{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module UHF.Prelude
    ( module X
    , Prelude.seq
    , identity
    , error
    , unreachable
    , todo
    , trace
    , trace_show_id
    , trace_with_message
    , assert
    , putStr
    , putStrLn
    , hPutStr
    , hPutStrLn
    , putText
    , hPutText
    , putTextLn
    , hPutTextLn
    , convert_str
    , show
    , Format (..)
    ) where

-- a lot of this is based on the stephen diehl's protolude (https://www.stephendiehl.com/posts/protolude.html)

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Function (id)
import qualified Data.Text
import qualified Data.Text.IO
import qualified Debug.Trace
import qualified System.IO
import qualified UHF.Source.FormattedString
import qualified Prelude

import GHC.Float as X (Double, Float)
import GHC.Generics as X (Generic)
import GHC.IO as X (IO)
import GHC.Num as X (Integer, Num (..))
import GHC.Real as X (Fractional (..), Integral (..), Ratio (..), Rational, Real (..), RealFrac (..), even, fromIntegral, odd, (^), (^^))
import GHC.Show as X (Show)
import GHC.Stack as X (HasCallStack)

import Data.Bits as X (Bits (..))
import Data.Bool as X (Bool (..), not, otherwise, (&&), (||))
import Data.Char as X (Char)
import Data.Either as X (Either (..), either, isLeft, isRight)
import Data.Function as X (const, flip, on, ($), (&), (.))
import Data.Int as X (Int)
import Data.Maybe as X (Maybe (..), catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybe, maybeToList)

import Data.List as X
    ( break
    , drop
    , dropWhile
    , filter
    , head
    , init
    , intercalate
    , intersperse
    , iterate
    , iterate'
    , last
    , map
    , repeat
    , replicate
    , reverse
    , span
    , splitAt
    , tail
    , take
    , takeWhile
    , uncons
    , unzip
    , unzip3
    , zip
    , zip3
    , zipWith
    , zipWith3
    , (++)
    )
import Data.Tuple as X
import Data.Void as X (Void, absurd)

import Data.Eq as X
import Data.Foldable as X hiding (foldl1, foldr1)
import Data.Functor as X (Functor, fmap, (<$), (<$>), (<&>))
import Data.Monoid as X (Monoid, mconcat, mempty)
import Data.Ord as X
import Data.Semigroup as X (Semigroup, sconcat, stimes, (<>))
import Data.Traversable as X (Traversable (..), for, forM, mapAccumL, mapAccumR)

import Safe as X (headDef, headMay, initDef, initMay, lastDef, lastMay, tailDef, tailMay)

import Data.IntMap as X (IntMap)
import Data.IntSet as X (IntSet)
import Data.List.NonEmpty as X (NonEmpty (..))
import Data.Map as X (Map)
import Data.Sequence as X (Seq)
import Data.Set as X (Set)

import Data.Text as X (Text)

import Control.Monad.Except as X (Except, ExceptT (..), MonadError, runExcept, runExceptT)
import Control.Monad.Reader as X (MonadReader (ask, reader), Reader, ReaderT (..), runReader, runReaderT)
import Control.Monad.State as X
    ( MonadState (get, put, state)
    , State
    , StateT (..)
    , evalState
    , evalStateT
    , execState
    , execStateT
    , modify
    , modify'
    , runState
    , runStateT
    )
import Control.Monad.Trans as X (MonadTrans (lift))
import Control.Monad.Writer as X
    ( MonadWriter (tell, writer)
    , Writer
    , WriterT (..)
    , execWriter
    , execWriterT
    , mapWriter
    , mapWriterT
    , runWriter
    , runWriterT
    )

import Control.Applicative as X (Applicative, pure, (<*>))
import Control.Monad as X (Monad, when, zipWithM, (>>), (>>=))

import System.Environment as X (getArgs, getProgName)
import System.Exit as X (exitFailure, exitSuccess)
import System.IO as X (FilePath, Handle, stderr, stdin, stdout)

import Test.Tasty as X (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit as X (Assertion, assertBool, assertFailure, testCase, (@=?), (@?=))
import Test.Tasty.TH as X (testGroupGenerator)

-- these are also mostly taken from the protolude, especially the Print class and the ConvertString class

identity :: a -> a
identity = Data.Function.id

error :: HasCallStack => Prelude.String -> a
error = Prelude.error

assert :: HasCallStack => Bool -> Prelude.String -> a -> a
assert True _ a = a
assert False msg _ = error msg

unreachable :: HasCallStack => a
unreachable = Prelude.error "unreachable code reached"

{-# WARNING todo "'todo'" #-}
todo :: HasCallStack => a
todo = Prelude.error "not implemented yet"

{-# WARNING trace "'trace'" #-}
trace :: Prelude.String -> a -> a
trace = Debug.Trace.trace

{-# WARNING trace_show_id "'trace_show_id'" #-}
trace_show_id :: Show a => a -> a
trace_show_id = Debug.Trace.traceShowId

{-# WARNING trace_with_message "'trace_with_message'" #-}
trace_with_message :: Show a => Prelude.String -> a -> a
trace_with_message msg a = Debug.Trace.trace (msg ++ ": " ++ show a) a

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

putTextLn :: MonadIO m => Data.Text.Text -> m ()
putTextLn = putStrLn
{-# SPECIALIZE putTextLn :: Data.Text.Text -> IO () #-}

hPutTextLn :: MonadIO m => Handle -> Data.Text.Text -> m ()
hPutTextLn = hPutStrLn
{-# SPECIALIZE putTextLn :: Data.Text.Text -> IO () #-}

show :: (Show a, ConvertString Prelude.String b) => a -> b
show = convert_str . Prelude.show

class ConvertString a b where
    convert_str :: a -> b

instance ConvertString Prelude.String Prelude.String where convert_str = identity
instance ConvertString Prelude.String Data.Text.Text where convert_str = Data.Text.pack
instance ConvertString Prelude.String UHF.Source.FormattedString.FormattedString where
    convert_str = UHF.Source.FormattedString.Literal . Data.Text.pack

instance ConvertString Data.Text.Text Prelude.String where convert_str = Data.Text.unpack
instance ConvertString Data.Text.Text Data.Text.Text where convert_str = identity
instance ConvertString Data.Text.Text UHF.Source.FormattedString.FormattedString where convert_str = UHF.Source.FormattedString.Literal

-- cannot convert to other 2 string types without losing sgr information
instance ConvertString UHF.Source.FormattedString.FormattedString UHF.Source.FormattedString.FormattedString where convert_str = identity

class Format a where
    format :: a -> Text

instance Format Int where
    format = convert_str . Prelude.show
instance Format Integer where
    format = convert_str . Prelude.show
