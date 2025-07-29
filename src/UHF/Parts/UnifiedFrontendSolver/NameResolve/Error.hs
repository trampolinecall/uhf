{-# LANGUAGE ExistentialQuantification #-}

module UHF.Parts.UnifiedFrontendSolver.NameResolve.Error
    ( Error (..)
    , WithErrors
    ) where

-- TODO: clean up this and the 3 modules too

import UHF.Prelude

import qualified UHF.Compiler as Compiler
import qualified UHF.Diagnostic as Diagnostic
import UHF.Parts.UnifiedFrontendSolver.NameResolve.DeclAt
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver as TypeSolver
import UHF.Source.Located (Located (Located))
import UHF.Source.Span (Span)

type WithErrors = Compiler.WithDiagnostics Error Void
data Error
    = Error'CouldNotFind (Located Text)
    | Error'CouldNotFindIn (Maybe (Located Text)) (Located Text)
    | Error'DuplicateDecl Text DeclAt DeclAt
    | Error'NotAType Span Text
    | forall t. Error'SolveError (TypeSolver.SolveError t) -- i did this out of laziness :)

instance Diagnostic.ToError Error where
    to_error (Error'CouldNotFind (Located sp name)) = Diagnostic.Error (Just sp) ("could not find name '" <> name <> "'") [] []
    to_error (Error'CouldNotFindIn prev (Located sp name)) =
        let message =
                "could not find name '"
                    <> name
                    <> "'"
                    <> case prev of
                        Just (Located _ prev_name) -> "in '" <> prev_name <> "'"
                        Nothing -> ""
        in Diagnostic.Error (Just sp) message [] []
    to_error (Error'DuplicateDecl name decl_before decl_now) =
        let span = decl_at_span decl_now
        in Diagnostic.Error
            span
            ("duplicate declaration of " <> convert_str name <> "'")
            [ (decl_at_span decl_before, Diagnostic.MsgError, Just $ message_for_decl_before name decl_before)
            , (decl_at_span decl_now, Diagnostic.MsgError, Just $ message_for_decl_now name decl_now)
            ]
            []
        where
            decl_at_span (DeclAt sp) = Just sp
            decl_at_span ImplicitPrim = Nothing

            message_for_decl_before n (DeclAt _) = "'" <> convert_str n <> "' previously declared here"
            message_for_decl_before n ImplicitPrim = "'" <> convert_str n <> "' is implicitly declared as a primitive" -- TODO: reword this message (ideally when it is declared through the prelude import the message would be something like 'implicitly declared by prelude')
            message_for_decl_now n (DeclAt _) = "'" <> convert_str n <> "' redefined here"
            message_for_decl_now n ImplicitPrim = "'" <> convert_str n <> "' is implicitly declared as a primitive" -- TODO: reword this message (ideally when it is declared through the prelude import the message would be something like 'implicitly declared by prelude')
    to_error (Error'NotAType sp instead) = Diagnostic.Error (Just sp) ("not a type: got " <> instead) [] []
    to_error (Error'SolveError se) = Diagnostic.to_error se
