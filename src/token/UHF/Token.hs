{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module UHF.Token
    ( Token(..)
    , LToken
    , IntLitBase(..)

    , Decimal(..)
    , format_tok
    ) where

import qualified UHF.IO.Location as Location

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

type LToken = Location.Located Token
data Token
    = OParen
    | CParen
    | OBrack
    | CBrack
    | Comma
    | Equal
    | Colon

    | Root
    | Let
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
    | EOF
    deriving (Show, Eq, Data.Data)

format_tok :: Token -> String
format_tok OParen = "'('"
format_tok CParen = "')'"
format_tok OBrack = "'['"
format_tok CBrack = "']'"
format_tok Comma = "','"
format_tok Equal = "'='"
format_tok Colon = "':'"

format_tok Root = "'root'"
format_tok Let = "'let'"
format_tok Type = "'type'"
format_tok Data = "'data'"
format_tok Under = "'under'"
format_tok If = "'if'"
format_tok Else = "'else'"
format_tok Case = "'case'"

format_tok (SymbolIdentifier _) = "symbol identifier"
format_tok (AlphaIdentifier _) = "alphabetic identifier"

format_tok (CharLit _) = "character literal"
format_tok (StringLit _) = "string literal"
format_tok (IntLit _ _) = "integer literal"
format_tok (FloatLit _) = "floating point literal"
format_tok (BoolLit _) = "bool literal"

format_tok OBrace = "'{'"
format_tok CBrace = "'}'"
format_tok Semicolon = "';'"
format_tok Indent = "indent"
format_tok Dedent = "dedent"
format_tok Newline = "newline"
format_tok EOF = "end of file"
