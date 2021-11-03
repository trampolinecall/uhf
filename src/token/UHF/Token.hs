module UHF.Token
    ( Token(..)
    , IntLitBase(..)
    ) where

data IntLitBase
    = Dec
    | Oct
    | Hex
    | Bin
    deriving Eq

data Token
    = OParen
    | CParen
    | OBrack
    | CBrack
    | Comma
    | Equal
    | DoubleColon

    | AlphaIdentifier String
    | SymbolIdentifier String

    | CharLit Char
    | StringLit String
    | IntLit IntLitBase Integer
    | FLoatLit Double
    | BoolLit Bool

    | Root
    | Let
    | Data
    | Under
    | If
    | Else
    | Case

    | OBrace
    | CBrace
    | Semicolon
    | Indent
    | Dedent
    | Newline
    deriving Eq
