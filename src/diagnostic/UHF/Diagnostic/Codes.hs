{-# LANGUAGE TemplateHaskell #-}

module UHF.Diagnostic.Codes where

import UHF.Diagnostic.Code
import UHF.Diagnostic.CodeTH

$(code Error 0 "bad-char")
$(code Error 1 "unclosed-multiline-comment")
$(code Error 2 "invalid-int-base")
$(code Error 3 "invalid-int-digit")
$(code Error 4 "non-decimal-float")
$(code Error 5 "missing-digits")
$(code Error 6 "unclosed-string-lit")
$(code Error 7 "unclosed-char-lit")
$(code Error 8 "invalid-char-lit")
$(code Error 9 "bad-dedent")
$(code Error 10 "invalid-double-colon")
