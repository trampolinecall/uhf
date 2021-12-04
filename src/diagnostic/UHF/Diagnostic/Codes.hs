{-# LANGUAGE TemplateHaskell #-}

module UHF.Diagnostic.Codes where

import UHF.Diagnostic.Code
import UHF.Diagnostic.CodeTH

$(code Error 0 "bad_char")
$(code Error 1 "unclosed_multiline_comment")
$(code Error 2 "invalid_int_base")
$(code Error 3 "invalid_int_digit")
$(code Error 4 "non_decimal_float")
$(code Error 5 "missing_digits")
$(code Error 6 "unclosed_string_lit")
$(code Error 7 "unclosed_char_lit")
$(code Error 8 "invalid_char_lit")
$(code Error 9 "bad_dedent")
$(code Error 10 "invalid_double_colon")
