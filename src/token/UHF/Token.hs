module UHF.Token
    ( Token(..)
    , IntLitBase(..)
    ) where

import qualified Data.Decimal as Decimal

data IntLitBase
    = Dec
    | Oct
    | Hex
    | Bin
    deriving (Show, Eq)

data Token
    = OParen
    | CParen
    | OBrack
    | CBrack
    | Comma
    | Equal
    | DoubleColon

    | SymbolIdentifier String

    | Root
    | Let
    | Data
    | Under
    | If
    | Else
    | Case

    | AlphaIdentifier String

    | CharLit Char
    | StringLit String
    | IntLit IntLitBase Integer
    | FloatLit Decimal.Decimal
    | BoolLit Bool

    | OBrace
    | CBrace
    | Semicolon
    | Indent
    | Dedent
    | Newline
    deriving (Show, Eq)
