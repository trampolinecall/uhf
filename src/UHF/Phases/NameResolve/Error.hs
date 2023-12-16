module UHF.Phases.NameResolve.Error
    ( Error (..)
    , WithErrors
    ) where

-- TODO: clean up this and the 3 modules too

import UHF.Prelude

import UHF.Phases.NameResolve.DeclAt
import UHF.Source.Located (Located (Located))
import UHF.Source.Span (Span)
import qualified UHF.Compiler as Compiler
import qualified UHF.Diagnostic as Diagnostic

type WithErrors = Compiler.WithDiagnostics Error Void
data Error
    = Error'CouldNotFind (Located Text)
    | Error'CouldNotFindIn (Maybe (Located Text)) (Located Text)
    | Error'MultipleDecls Text [DeclAt]
    | Error'NotAType Span Text

instance Diagnostic.ToError Error where
    to_error (Error'CouldNotFind (Located sp name)) = Diagnostic.Error (Just sp) ("could not find name '" <> name <> "'") [] []
    to_error (Error'CouldNotFindIn prev (Located sp name)) =
        let message =
                "could not find name '" <> name <> "'"
                    <> case prev of
                        Just (Located _ prev_name) -> "in '" <> prev_name <> "'"
                        Nothing -> ""
        in Diagnostic.Error (Just sp) message [] []

    to_error (Error'MultipleDecls name decl_ats) =
        let span = headMay $ mapMaybe decl_at_span decl_ats -- take the first span of the decl_ats; if there are no decl_ats with a span, then this will be Ntohing
        in Diagnostic.Error
            span
            (show (length decl_ats) <> " declarations of '" <> convert_str name <> "'")
            (map (\ at -> (decl_at_span at, Diagnostic.MsgError, decl_at_message name at)) decl_ats)
            []
        where
            decl_at_span (DeclAt sp) = Just sp
            decl_at_span ImplicitPrim = Nothing
            decl_at_message _ (DeclAt _) = Nothing
            decl_at_message n ImplicitPrim = Just $ "'" <> convert_str n <> "' is implicitly declared as a primitive" -- TODO: reword this message (ideally when it is declared through the prelude import the message would be something like 'implicitly declared by prelude')

    to_error (Error'NotAType sp instead) =
        Diagnostic.Error (Just sp) ("not a type: got " <> instead) [] []
