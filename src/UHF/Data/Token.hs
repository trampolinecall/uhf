{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file -fshow-error-context #-}

module UHF.Data.Token where

import UHF.Prelude hiding (Bool, Char, Float, Int)
import qualified UHF.Prelude

import qualified Data.Data as Data
import qualified Language.Haskell.TH.Syntax as TH.Syntax (Lift)

import qualified UHF.Data.Token.TH as TH
import UHF.Source.EqIgnoringSpans
import UHF.Source.Located (Located)

data IntBase
    = Dec
    | Oct
    | Hex
    | Bin
    deriving (Show, Eq, Ord, Generic, Data.Data, EqIgnoringSpans, TH.Syntax.Lift)

$( TH.generate
    [ TH.tt "OParen" [] "'('" [|"'('"|]
    , TH.tt "CParen" [] "')'" [|"')'"|]
    , TH.tt "OBrack" [] "'['" [|"'['"|]
    , TH.tt "CBrack" [] "']'" [|"']'"|]
    , TH.tt "OBrace" [] "'{'" [|"'{'"|]
    , TH.tt "CBrace" [] "'}'" [|"'}'"|]
    , TH.tt "Semicolon" [] "';'" [|"';'"|]
    , TH.tt "Comma" [] "','" [|"','"|]
    , TH.tt "Char" [[t|UHF.Prelude.Char|]] "character literal" [|\c -> "'" <> convert_str [c] <> "'"|]
    , TH.tt "String" [[t|Text|]] "string literal" [|\s -> "'\"" <> convert_str s <> "\"'"|]
    , TH.tt "Int" [[t|IntBase|], [t|Integer|]] "integer literal" [|\_ i -> "'" <> show i <> "'"|]
    , TH.tt "Float" [[t|Rational|]] "floating point literal" [|\f -> "'" <> show f <> "'"|]
    , TH.tt "Bool" [[t|UHF.Prelude.Bool|]] "bool literal" [|\b -> "'" <> if b then "true" else "false" <> "'"|]
    , TH.tt "SymbolIdentifier" [[t|Text|]] "symbol identifier" [|\i -> convert_str $ "symbol identifier '" <> i <> "'"|]
    , TH.tt "AlphaIdentifier" [[t|Text|]] "alphabetic identifier" [|\i -> convert_str $ "alphabetic identifier '" <> i <> "'"|]
    , TH.tt "KeywordIdentifier" [[t|Text|]] "keyword identifier" [|\i -> convert_str $ "keyword identifier '" <> i <> ":'"|] -- TODO: implement lexing for this
    , TH.tt "Equal" [] "'='" [|"'='"|]
    , TH.tt "Colon" [] "':'" [|"':'"|]
    , TH.tt "Arrow" [] "'->'" [|"'->'"|]
    , TH.tt "Hash" [] "'#'" [|"'#'"|]
    , TH.tt "At" [] "'@'" [|"'@'"|]
    , TH.tt "Question" [] "'?'" [|"'?'"|]
    , TH.tt "Backslash" [] "'\\'" [|"'\\'"|]
    , TH.tt "DoubleColon" [] "'::'" [|"'::'"|]
    , TH.tt "Caret" [] "'^'" [|"'^'"|] -- TODO: implement lexing for this and remove this eventually
    , TH.tt "Backtick" [] "'`'" [|"'`'"|] -- TODO: implement lexing for this
    , TH.tt "Underscore" [] "'_'" [|"'_'"|]
    , TH.tt "Root" [] "'root'" [|"'root'"|]
    , TH.tt "Let" [] "'let'" [|"'let'"|]
    , TH.tt "LetRec" [] "'letrec'" [|"'letrec'"|]
    , TH.tt "Where" [] "'where'" [|"'where'"|]
    , TH.tt "TypeSyn" [] "'typesyn'" [|"'typesyn'"|]
    , TH.tt "Data" [] "'data'" [|"'data'"|]
    , TH.tt "Impl" [] "'impl'" [|"'impl'"|]
    , TH.tt "If" [] "'if'" [|"'if'"|]
    , TH.tt "Then" [] "'then'" [|"'then'"|]
    , TH.tt "Else" [] "'else'" [|"'else'"|]
    , TH.tt "Match" [] "'match'" [|"'match'"|]
    , TH.tt "EOF" [] "end of file" [|"end of file"|]
    ]
 )

type LToken = Located Token

is_tt :: TokenType -> Token -> UHF.Prelude.Bool
is_tt ty tok = ty == to_token_type tok
