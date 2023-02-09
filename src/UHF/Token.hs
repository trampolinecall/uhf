{-# LANGUAGE FlexibleInstances #-}

module UHF.Token
    ( BaseToken(..)
    , SingleTypeToken(..)

    , LInternalToken
    , LToken
    , InternalToken
    , Token
    , TokenType

    , IntLitBase(..)

    , to_token_type
    ) where

import UHF.Util.Prelude

import qualified UHF.IO.Location as Location

import qualified Data.Text as Text

data IntLitBase
    = Dec
    | Oct
    | Hex
    | Bin
    deriving (Show, Eq)

type LToken = Location.Located Token
type LInternalToken = Location.Located InternalToken

type InternalToken = BaseToken (Location.Located Text) Void Char Text IntLitBase Integer Rational Bool
type Token = BaseToken [Location.Located Text] () Char Text IntLitBase Integer Rational Bool
type TokenType = BaseToken () () () () () () () ()

data SingleTypeToken
    = OParen
    | CParen
    | OBrack
    | CBrack
    | Comma
    | Equal
    | Colon
    | Arrow

    | DoubleColon

    | Root
    | Let
    | Let'
    | Type
    | Data
    | Under
    | If
    | Else
    | Case

    | OBrace
    | CBrace
    | Semicolon
    deriving (Show, Eq)

data BaseToken identifier eof char_lit_data string_lit_data intlit_base int_lit_data float_lit_data bool_lit_data
    = SingleTypeToken SingleTypeToken

    | CharLit char_lit_data
    | StringLit string_lit_data
    | IntLit intlit_base int_lit_data
    | FloatLit float_lit_data
    | BoolLit bool_lit_data

    | SymbolIdentifier identifier
    | AlphaIdentifier identifier

    | EOF eof
    deriving (Show, Eq)

instance Format SingleTypeToken where
    format OParen = "'('"
    format CParen = "')'"
    format OBrack = "'['"
    format CBrack = "']'"
    format Comma = "','"
    format Equal = "'='"
    format Colon = "':'"
    format Arrow = "'->'"
    format DoubleColon = "'::'"

    format Root = "'root'"
    format Let = "'let'"
    format Let' = "'let''" -- TODO: print this more nicely
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

    format (SymbolIdentifier parts) = convert_str $ "symbol identifier '" <> Text.intercalate "::" (map Location.unlocate parts) <> "'"
    format (AlphaIdentifier parts) = convert_str $ "alphabetic identifier '" <> Text.intercalate "::" (map Location.unlocate parts) <> "'"

    format (EOF ()) = "end of file"

to_token_type :: BaseToken identifier eof char_lit_data string_lit_data intlit_base int_lit_data float_lit_data bool_lit_data -> TokenType
to_token_type (SingleTypeToken stt) = SingleTypeToken stt

to_token_type (CharLit _) = CharLit ()
to_token_type (StringLit _) = StringLit ()
to_token_type (IntLit _ _) = IntLit () ()
to_token_type (FloatLit _) = FloatLit ()
to_token_type (BoolLit _) = BoolLit ()

to_token_type (SymbolIdentifier _) = SymbolIdentifier ()
to_token_type (AlphaIdentifier _) = AlphaIdentifier ()

to_token_type (EOF _) = EOF ()
