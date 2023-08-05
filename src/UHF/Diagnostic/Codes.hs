module UHF.Diagnostic.Codes where

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
$(error_code 0009 "invalid_double_colon")

$(error_code 0100 "parse_error")

$(error_code 0200 "multiple_decls")
$(error_code 0201 "binding_lhs_path")
$(error_code 0202 "path_in_type_name")
$(error_code 0203 "path_in_variant_name")
$(error_code 0204 "path_in_field_name")
$(error_code 0205 "tuple0")
$(error_code 0206 "tuple1")

$(error_code 0300 "undef_name")

$(error_code 0400 "not_a_type")
$(error_code 0401 "type_mismatch")
$(error_code 0402 "occurs_check")
$(error_code 0403 "ambiguous_type")
$(error_code 0404 "does_not_take_type_argument")
$(error_code 0405 "wrong_type_argument")

$(error_code 0500 "hole")

$(error_code 0600 "incomplete_patterns")
$(error_code 0601 "useless_pattern")
