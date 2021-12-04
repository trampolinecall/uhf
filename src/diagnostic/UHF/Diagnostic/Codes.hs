{-# LANGUAGE TemplateHaskell #-}

module UHF.Diagnostic.Codes where

import UHF.Diagnostic.Code
import UHF.Diagnostic.CodeTH

$(code Error 0 "bad-char")
