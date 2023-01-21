{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module UHF.Token
    ( BaseToken(..)
    , SingleTypeToken(..)

    , LRawToken
    , LToken
    , RawToken
    , Token
    , TokenType

    , NLPhysical(..), NLLogical(..)

    , IntLitBase(..)
    ) where

import UHF.Util.Prelude

import qualified UHF.IO.Location as Location

import qualified Data.Text as Text
import qualified Data.Data as Data

data IntLitBase
    = Dec
    | Oct
    | Hex
    | Bin
    deriving (Show, Eq, Data.Data)

type LToken = Location.Located Token
type LRawToken = Location.Located RawToken

type RawToken = BaseToken () Text Void Char Text IntLitBase Integer Rational Bool
type Token = BaseToken Void [Text] () Char Text IntLitBase Integer Rational Bool
type TokenType = BaseToken () () () () () () () () ()

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

    | OBrace
    | CBrace
    | Semicolon
    deriving (Show, Eq, Data.Data)

data BaseToken double_colon identifier eof char_lit_data string_lit_data intlit_base int_lit_data float_lit_data bool_lit_data
    = SingleTypeToken SingleTypeToken

    | CharLit char_lit_data
    | StringLit string_lit_data
    | IntLit intlit_base int_lit_data
    | FloatLit float_lit_data
    | BoolLit bool_lit_data

    | DoubleColon double_colon

    | SymbolIdentifier identifier
    | AlphaIdentifier identifier

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

    format OBrace = "'{'"
    format CBrace = "'}'"
    format Semicolon = "';'"

instance Format TokenType where
    format (SingleTypeToken s) = format s

    format (CharLit ()) = "character literal"
    format (StringLit ()) = "string literal"
    format (IntLit () ()) = "integer literal"
    format (FloatLit ()) = "floating point literal"
    format (BoolLit ()) = "bool literal"

    format (DoubleColon ()) = "'::'"

    format (SymbolIdentifier ()) = "symbol identifier"
    format (AlphaIdentifier ()) = "alphabetic identifier"

    format (EOF ()) = "end of file"

instance Format Token where
    format (SingleTypeToken s) = format s

    format (CharLit c) = "'" <> convert_str [c] <> "'"
    format (StringLit s) = "'\"" <> convert_str s <> "\"'"
    format (IntLit _ i) = "'" <> show i <> "'"
    format (FloatLit f) = "'" <> show f <> "'"
    format (BoolLit b) = "'" <> if b then "true" else "false" <> "'"

    format (DoubleColon void) = absurd void

    format (SymbolIdentifier parts) = convert_str $ "symbol identifier '" <> Text.intercalate "::" parts <> "'"
    format (AlphaIdentifier parts) = convert_str $ "alphabetic identifier '" <> Text.intercalate "::" parts <> "'"

    format (EOF _) = "end of file"
