{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module UHF.Token
    ( BaseToken(..)
    , SingleTypeToken(..)

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

data SingleTypeToken
    = OParen
    | CParen
    | OBrack
    | CBrack
    | Comma
    | Equal
    | Colon
    | Arrow

    | Root
    | Let
    | Type
    | Data
    | Under
    | If
    | Else
    | Case

    | CharLit Char
    | StringLit String
    | IntLit IntLitBase Integer
    | FloatLit Decimal.Decimal
    | BoolLit Bool
    deriving (Show, Eq, Data.Data)

data BaseToken doublecolon identifier eof indentation newline backslash
    = SingleTypeToken SingleTypeToken

    | DoubleColon doublecolon

    | SymbolIdentifier identifier
    | AlphaIdentifier identifier

    | OBrace
    | CBrace
    | Semicolon
    | Backslash backslash
    | Indent indentation
    | Dedent indentation
    | Newline newline
    | EOF eof
    deriving (Show, Eq, Data.Data)

format_sttok :: SingleTypeToken -> String
format_sttok OParen = "'('"
format_sttok CParen = "')'"
format_sttok OBrack = "'['"
format_sttok CBrack = "']'"
format_sttok Comma = "','"
format_sttok Equal = "'='"
format_sttok Colon = "':'"
format_sttok Arrow = "'->'"

format_sttok Root = "'root'"
format_sttok Let = "'let'"
format_sttok Type = "'type'"
format_sttok Data = "'data'"
format_sttok Under = "'under'"
format_sttok If = "'if'"
format_sttok Else = "'else'"
format_sttok Case = "'case'"

format_sttok (CharLit _) = "character literal"
format_sttok (StringLit _) = "string literal"
format_sttok (IntLit _ _) = "integer literal"
format_sttok (FloatLit _) = "floating point literal"
format_sttok (BoolLit _) = "bool literal"

format_tok :: BaseToken doublecolon identifier eof indentation newline backslash -> String
format_tok (SingleTypeToken s) = format_sttok s

format_tok (DoubleColon _) = "':'"

format_tok (SymbolIdentifier _) = "symbol identifier"
format_tok (AlphaIdentifier _) = "alphabetic identifier"

format_tok OBrace = "'{'"
format_tok CBrace = "'}'"
format_tok Semicolon = "';'"
format_tok (Backslash _) = "'\\'"
format_tok (Indent _) = "indent"
format_tok (Dedent _) = "dedent"
format_tok (Newline _) = "newline"
format_tok (EOF _) = "end of file"
