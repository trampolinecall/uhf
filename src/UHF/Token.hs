{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module UHF.Token
    ( BaseToken(..)

    , LUnprocessedToken
    , LTokenWithIndentation
    , LNormalToken
    , UnprocessedToken
    , TokenWithIndentation
    , NormalToken

    , NLPhysical(..), NLLogical(..)

    , IntLitBase(..)

    , Decimal.Decimal
    , format_tok
    ) where

import qualified UHF.IO.Location as Location

import qualified Data.Decimal as Decimal
import qualified Data.Data as Data
import qualified Data.Void as Void

deriving instance Data.Data Decimal.Decimal -- TODO: maybe not have an orphan instance

data IntLitBase
    = Dec
    | Oct
    | Hex
    | Bin
    deriving (Show, Eq, Data.Data)

type LNormalToken = Location.Located NormalToken
type LTokenWithIndentation = Location.Located TokenWithIndentation
type LUnprocessedToken = Location.Located UnprocessedToken

type UnprocessedToken = BaseToken () String Void.Void Void.Void NLPhysical ()
type TokenWithIndentation = BaseToken () String Void.Void () NLLogical Void.Void
type NormalToken = BaseToken Void.Void [String] () () NLLogical Void.Void

data NLLogical = NLLogical deriving (Show, Eq, Data.Data)
data NLPhysical = NLPhysical deriving (Show, Eq, Data.Data)

data BaseToken doublecolon identifier eof indentation newline backslash
    = OParen
    | CParen
    | OBrack
    | CBrack
    | Comma
    | Equal
    | Colon
    | Arrow

    | DoubleColon doublecolon

    | Root
    | Let
    | Type
    | Data
    | Under
    | If
    | Else
    | Case

    | SymbolIdentifier identifier
    | AlphaIdentifier identifier

    | CharLit Char
    | StringLit String
    | IntLit IntLitBase Integer
    | FloatLit Decimal.Decimal
    | BoolLit Bool

    | OBrace
    | CBrace
    | Semicolon
    | Backslash backslash
    | Indent indentation
    | Dedent indentation
    | Newline newline
    | EOF eof
    deriving (Show, Eq, Data.Data)

format_tok :: BaseToken doublecolon identifier eof indentation newline backslash -> String
format_tok OParen = "'('"
format_tok CParen = "')'"
format_tok OBrack = "'['"
format_tok CBrack = "']'"
format_tok Comma = "','"
format_tok Equal = "'='"
format_tok Colon = "':'"
format_tok (DoubleColon _) = "':'"
format_tok Arrow = "'->'"

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
format_tok (Backslash _) = "'\\'"
format_tok (Indent _) = "indent"
format_tok (Dedent _) = "dedent"
format_tok (Newline _) = "newline"
format_tok (EOF _) = "end of file"
