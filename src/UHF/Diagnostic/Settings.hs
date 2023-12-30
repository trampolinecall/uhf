module UHF.Diagnostic.Settings where

newtype Settings = Settings ReportStyle

data ReportStyle = ASCII | Unicode
