module UHF.Compiler.DiagnosticSettings where

data ColorsNeeded = Colors | NoColors | AutoDetect
data DiagnosticSettings
    = DiagnosticSettings
        { _colors_needed :: ColorsNeeded
        }

