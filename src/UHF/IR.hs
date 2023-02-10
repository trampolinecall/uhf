module UHF.IR
    ( DeclKey
    , Decl(..)
    , Module(..)
    , ValueKey
    , Value(..)
    , Expr(..)
    ) where

import UHF.Util.Prelude

import qualified Arena

import qualified Data.Map as Map

newtype DeclKey = DeclKey Int deriving Show
instance Arena.Key DeclKey where
    make_key = DeclKey
    unmake_key (DeclKey i) = i

data Decl = Decl'Module Module deriving Show
data Module = Module (Map.Map Text DeclKey) (Map.Map Text ValueKey) deriving Show

newtype ValueKey = ValueKey Int deriving Show
instance Arena.Key ValueKey where
    make_key = ValueKey
    unmake_key (ValueKey i) = i

data Value identifier = Value (Expr identifier) deriving Show

data Expr identifier
    = Expr'Identifier identifier
    | Expr'Char Char
    | Expr'String Text
    | Expr'Int Integer
    | Expr'Float Rational
    | Expr'Bool Bool
    deriving (Eq, Show)
