{-# LANGUAGE FlexibleInstances #-}

module UHF.Diagnostic
    ( Diagnostic(..)
    , Section
    , section_contents

    , IsDiagnostic(..)
    , ToSection(..)
    , to_section
    , report
    , report_diagnostics
    ) where

import UHF.Util.Prelude

import qualified UHF.Diagnostic.FormattedString as FormattedString
import qualified UHF.Diagnostic.Codes.Code as Code
import qualified UHF.Diagnostic.Colors as Colors
import qualified UHF.Diagnostic.Line as Line

import qualified UHF.IO.Location as Location

import qualified Data.Text as Text
import qualified System.IO as IO

data Diagnostic = Diagnostic Code.Code (Maybe Location.Span) [Section]

class IsDiagnostic d where
    to_diagnostic :: d -> Diagnostic

newtype Section = Section { section_contents :: [Line.Line] }
class ToSection s where
    to_section' :: s -> [Line.Line]
instance ToSection [Line.Line] where
    to_section' = identity

to_section :: ToSection s => s -> Section
to_section = Section . to_section'

report :: Handle -> Diagnostic -> IO ()
report handle (Diagnostic code m_sp sections) =
    let header = FormattedString.make_formatted_string $
            case Code.code_type code of
                Code.Error -> [(Colors.error, "error")]
                Code.Warning -> [(Colors.warning, "warning")]
                Code.DebugMessage -> [(Colors.debug_message, "debug message")]
                Code.InternalError -> [(Colors.error, "internal error")]
            ++
            case m_sp of
                Just sp -> [([], " at "), (Colors.file_path, format sp)]
                Nothing -> []

        footer =
            case Code.code_code_desc code of
                Just (c, d) -> Just $ FormattedString.make_formatted_string [([], "==> ["), (Colors.diag_code, c), ([], "] "), (Colors.diag_desc, d)]
                Nothing -> Nothing

        section_lines = concatMap (\ (Section l) -> l) sections
        indent =
            if null section_lines
                then 4
                else maximum $ map (Text.length . Line.prefix) section_lines

        p_fmtstr = FormattedString.render_formatted_string handle FormattedString.AutoDetect

        p_line l =
            let pre = Line.prefix l
                sep = Line.separ l
                contents = Line.contents l
            in
            hPutStr handle (replicate (indent - Text.length pre) ' ') >>
            hPutStr handle pre >>
            hPutStr handle [' ', sep, ' '] >>
            p_fmtstr contents >>
            hPutText handle "\n"
    in
    p_fmtstr header >>
    hPutText handle "\n" >>
    mapM_ p_line section_lines >>
    maybe (pure ()) (\ f -> hPutStr handle (replicate indent ' ') >> p_fmtstr f >> IO.hPutStr handle "\n") footer

report_diagnostics :: Handle -> [Diagnostic] -> IO ()
report_diagnostics handle = mapM_ (report handle)
