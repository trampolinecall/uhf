module UHF.Diagnostic.Settings where

data ColorsNeeded = Colors | NoColors | AutoDetect
data Settings
    = Settings
        { _colors_needed :: ColorsNeeded
        }

