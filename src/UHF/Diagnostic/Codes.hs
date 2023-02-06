module UHF.Diagnostic.Codes where

import UHF.Diagnostic.Codes.Code
import UHF.Diagnostic.Codes.CodeTH

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

$(code Error 100 "parse_error")
$(code Error 101 "not_implemented")

$(code Error 200 "symbol_redefinition")

$(code Error 300 "multi_iden")
$(code Error 301 "undef_name")
