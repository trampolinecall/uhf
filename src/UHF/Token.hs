{-# LANGUAGE DeriveDataTypeable #-}
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
    , TokenType

    , NLPhysical(..), NLLogical(..)

    , IntLitBase(..)
    ) where

import UHF.Util.Prelude

import qualified UHF.IO.Location as Location

import qualified Data.Data as Data
import qualified Data.Void as Void

data IntLitBase
    = Dec
    | Oct
    | Hex
    | Bin
    deriving (Show, Eq, Data.Data)

type LNormalToken = Location.Located NormalToken
type LTokenWithIndentation = Location.Located TokenWithIndentation
type LUnprocessedToken = Location.Located UnprocessedToken

type UnprocessedToken = BaseToken () Text Void Void.Void NLPhysical () Char Text IntLitBase Integer Rational Bool
type TokenWithIndentation = BaseToken () Text Void () NLLogical Void.Void Char Text IntLitBase Integer Rational Bool
type NormalToken = BaseToken Void [Text] () () NLLogical Void.Void Char Text IntLitBase Integer Rational Bool
type TokenType = BaseToken () () () () () () () () () () () ()

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
    deriving (Show, Eq, Data.Data)

data BaseToken double_colon identifier eof indentation newline backslash char_lit_data string_lit_data intlit_base int_lit_data float_lit_data bool_lit_data
    = SingleTypeToken SingleTypeToken

    | CharLit char_lit_data
    | StringLit string_lit_data
    | IntLit intlit_base int_lit_data
    | FloatLit float_lit_data
    | BoolLit bool_lit_data

    | DoubleColon double_colon

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

instance Format SingleTypeToken where
    format OParen = "'('"
    format CParen = "')'"
    format OBrack = "'['"
    format CBrack = "']'"
    format Comma = "','"
    format Equal = "'='"
    format Colon = "':'"
    format Arrow = "'->'"

    format Root = "'root'"
    format Let = "'let'"
    format Type = "'type'"
    format Data = "'data'"
    format Under = "'under'"
    format If = "'if'"
    format Else = "'else'"
    format Case = "'case'"

instance Format (BaseToken double_colon identifier eof indentation newline backslash char_lit_data string_lit_data intlit_base int_lit_data float_lit_data bool_lit_data) where
    format (SingleTypeToken s) = format s

    format (CharLit _) = "character literal"
    format (StringLit _) = "string literal"
    format (IntLit _ _) = "integer literal"
    format (FloatLit _) = "floating point literal"
    format (BoolLit _) = "bool literal"

    format (DoubleColon _) = "'::'"

    format (SymbolIdentifier _) = "symbol identifier"
    format (AlphaIdentifier _) = "alphabetic identifier"

    format OBrace = "'{'"
    format CBrace = "'}'"
    format Semicolon = "';'"
    format (Backslash _) = "'\\'"
    format (Indent _) = "indent"
    format (Dedent _) = "dedent"
    format (Newline _) = "newline"
    format (EOF _) = "end of file"
