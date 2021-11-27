module UHF.RawToken
    ( Token(..)
    ) where

import qualified Data.Decimal as Decimal
import qualified UHF.Token as Token

data Token
    = OParen
    | CParen
    | OBrack
    | CBrack
    | Comma
    | Equal
    | DoubleColon

    | Root
    | Let
    | Data
    | Under
    | If
    | Else
    | Case

    | SymbolIdentifier String
    | AlphaIdentifier String

    | CharLit Char
    | StringLit String
    | IntLit Token.IntLitBase Integer
    | FloatLit Decimal.Decimal
    | BoolLit Bool

    | OBrace
    | CBrace
    | Semicolon
    | Indent
    | Dedent
    | Newline
    deriving (Show, Eq)
