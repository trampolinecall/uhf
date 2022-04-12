module UHF.AST.Decl where

import qualified UHF.AST.Type as Type
import qualified UHF.AST.Expr as Expr

data Decl
    = Type String Type.Type
    | Define String Expr.Expr -- TODO: this should eventually be a pattern
