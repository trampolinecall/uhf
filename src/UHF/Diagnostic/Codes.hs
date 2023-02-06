module UHF.Diagnostic.Codes where

import UHF.Diagnostic.Codes.Code
import UHF.Diagnostic.Codes.CodeTH

$(error_code 0000 "bad_char")
$(error_code 0001 "unclosed_multiline_comment")
$(error_code 0002 "invalid_int_base")
$(error_code 0003 "invalid_int_digit")
$(error_code 0004 "non_decimal_float")
$(error_code 0005 "missing_digits")
$(error_code 0006 "unclosed_string_lit")
$(error_code 0007 "unclosed_char_lit")
$(error_code 0008 "invalid_char_lit")
$(error_code 0009 "bad_dedent")
$(error_code 0010 "invalid_double_colon")

$(error_code 0100 "parse_error")
$(error_code 0101 "binding_lhs_path")
$(error_code 0102 "not_implemented")

$(error_code 0200 "symbol_redefinition")

$(error_code 0300 "multi_iden")
$(error_code 0301 "undef_name")
