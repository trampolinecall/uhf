{-# LANGUAGE FlexibleInstances #-}

module UHF.Token
    ( BaseToken(..)
    , SingleTypeToken(..)

    , LInternalToken
    , LToken
    , InternalToken
    , Token
    , TokenType

    , IntBase(..)

    , to_token_type
    ) where

import UHF.Util.Prelude

import qualified UHF.IO.Location as Location

import qualified Data.Text as Text

data IntBase
    = Dec
    | Oct
    | Hex
    | Bin
    deriving (Show, Eq)

type LToken = Location.Located Token
type LInternalToken = Location.Located InternalToken

type InternalToken = BaseToken (Location.Located Text) Void Char Text IntBase Integer Rational Bool
type Token = BaseToken [Location.Located Text] () Char Text IntBase Integer Rational Bool
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
    | LetRec
    | Type -- TODO: change to TypeSyn
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

    | Char char_lit_data
    | String string_lit_data
    | Int intlit_base int_lit_data
    | Float float_lit_data
    | Bool bool_lit_data

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
    format LetRec = "'letrec'"
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

    format (Char ()) = "character literal"
    format (String ()) = "string literal"
    format (Int () ()) = "integer literal"
    format (Float ()) = "floating point literal"
    format (Bool ()) = "bool literal"

    format (SymbolIdentifier ()) = "symbol identifier"
    format (AlphaIdentifier ()) = "alphabetic identifier"

    format (EOF ()) = "end of file"

instance Format Token where
    format (SingleTypeToken s) = format s

    format (Char c) = "'" <> convert_str [c] <> "'"
    format (String s) = "'\"" <> convert_str s <> "\"'"
    format (Int _ i) = "'" <> show i <> "'"
    format (Float f) = "'" <> show f <> "'"
    format (Bool b) = "'" <> if b then "true" else "false" <> "'"

    format (SymbolIdentifier parts) = convert_str $ "symbol identifier '" <> Text.intercalate "::" (map Location.unlocate parts) <> "'"
    format (AlphaIdentifier parts) = convert_str $ "alphabetic identifier '" <> Text.intercalate "::" (map Location.unlocate parts) <> "'"

    format (EOF ()) = "end of file"

to_token_type :: BaseToken identifier eof char_lit_data string_lit_data intlit_base int_lit_data float_lit_data bool_lit_data -> TokenType
to_token_type (SingleTypeToken stt) = SingleTypeToken stt

to_token_type (Char _) = Char ()
to_token_type (String _) = String ()
to_token_type (Int _ _) = Int () ()
to_token_type (Float _) = Float ()
to_token_type (Bool _) = Bool ()

to_token_type (SymbolIdentifier _) = SymbolIdentifier ()
to_token_type (AlphaIdentifier _) = AlphaIdentifier ()

to_token_type (EOF _) = EOF ()
