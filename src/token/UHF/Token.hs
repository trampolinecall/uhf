{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module UHF.Token
    ( Token(..)
    , IntLitBase(..)

    , Decimal(..)
    ) where

import qualified Data.Decimal as Decimal
import qualified Data.Data as Data

deriving instance Data.Data Decimal.Decimal -- TODO: maybe not have an orphan instance
newtype Decimal = Decimal Decimal.Decimal deriving (Show, Eq, Data.Data)

data IntLitBase
    = Dec
    | Oct
    | Hex
    | Bin
    deriving (Show, Eq, Data.Data)

data Token
    = OParen
    | CParen
    | OBrack
    | CBrack
    | Comma
    | Equal

    | Root
    | Let
    | Define
    | Type
    | Data
    | Under
    | If
    | Else
    | Case

    | SymbolIdentifier [String]
    | AlphaIdentifier [String]

    | CharLit Char
    | StringLit String
    | IntLit IntLitBase Integer
    | FloatLit Decimal
    | BoolLit Bool

    | OBrace
    | CBrace
    | Semicolon
    | Indent
    | Dedent
    | Newline
    deriving (Show, Eq, Data.Data)
