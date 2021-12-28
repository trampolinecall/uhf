module UHF.Compiler
    ( compile
    ) where

import qualified UHF.IO.File as File
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Lexer as Lexer

compile :: Phase File.File (Maybe ())
compile = lex_phase

type Phase a b = a -> ([Diagnostic.Diagnostic], b)

link_phases :: Phase a b -> Phase b c -> Phase a c
link_phases phase1 phase2 a =
    let (diags1, b) = phase1 a
        (diags2, c) = phase2 b
    in (diags1 ++ diags2, c)

lex_phase :: Phase File.File (Maybe ())
lex_phase file =
    let (diagnostics, _) = Lexer.lex file
    in (map Diagnostic.to_diagnostic diagnostics, Nothing)
