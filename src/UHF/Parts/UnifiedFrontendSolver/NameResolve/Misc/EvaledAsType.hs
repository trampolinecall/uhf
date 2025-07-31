module UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.EvaledAsType (NotAType, evaled_as_type) where

import UHF.Prelude

import qualified UHF.Data.SIR as SIR
import qualified UHF.Diagnostic as Diagnostic
import UHF.Source.Span (Span)

data NotAType = NotAType Span Text

instance Diagnostic.ToError NotAType where
    to_error (NotAType sp instead) = Diagnostic.Error (Just sp) ("not a type: got " <> instead) [] []

evaled_as_type :: Span -> SIR.DeclRef t -> Either NotAType t
evaled_as_type sp d =
    case d of
        SIR.DeclRef'Module _ -> Left $ NotAType sp "a module"
        SIR.DeclRef'Type ty -> Right ty
        SIR.DeclRef'ExternPackage _ -> do Left $ NotAType sp "external package"
