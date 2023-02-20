module UHF.Diagnostic.Report.Style
    ( Style(..)
    , default_style
    ) where

import UHF.Util.Prelude

import System.Console.ANSI

data Style
    = Style
        { file_path_color, error_color, warning_color, debug_message_color, note_color, hint_color, code_color, desc_color :: [SGR]
        , und_other_color :: [SGR]

        , msg_error_char, msg_warning_char, msg_note_char, msg_hint_char, msg_error_char_top, msg_warning_char_top, msg_note_char_top, msg_hint_char_top :: Char
        , file_line_char, header_line_char, other_line_char :: Char

        , message_prefix_line, message_prefix_last :: Char
        , message_prefix :: Text
        }

default_style :: Style
default_style = Style
    { file_path_color = [bold, fg_bcyan]
    , error_color = [bold, fg_bred]
    , warning_color = [bold, fg_bmagenta]
    , debug_message_color = [bold]
    , note_color = [bold, fg_bgreen]
    , hint_color = [bold, fg_bblue]
    , code_color = [bold]
    , desc_color = [bold]

    , und_other_color = [bold]

    , msg_error_char = '^'
    , msg_warning_char = '^'
    , msg_note_char = '-'
    , msg_hint_char = '-'
    , msg_error_char_top = 'v'
    , msg_warning_char_top = 'v'
    , msg_note_char_top = '-'
    , msg_hint_char_top = '-'
    , file_line_char = '>'
    , header_line_char = '\\'
    , other_line_char = '|'

    , message_prefix_line = '|'
    , message_prefix_last = '`'
    , message_prefix = "--"
    }

bold, bg_white, bg_cyan, bg_magenta, bg_blue, bg_yellow, bg_green, bg_red, bg_black, bg_bwhite, bg_bcyan, bg_bmagenta, bg_bblue, bg_byellow, bg_bgreen, bg_bred, bg_bblack, fg_white, fg_cyan, fg_magenta, fg_blue, fg_yellow, fg_green, fg_red, fg_black, fg_bwhite, fg_bcyan, fg_bmagenta, fg_bblue, fg_byellow, fg_bgreen, fg_bred, fg_bblack, reset, normal :: SGR

bold = SetConsoleIntensity BoldIntensity
normal = SetConsoleIntensity NormalIntensity

reset = Reset

fg_bblack = SetColor Foreground Vivid Black
fg_bred = SetColor Foreground Vivid Red
fg_bgreen = SetColor Foreground Vivid Green
fg_byellow = SetColor Foreground Vivid Yellow
fg_bblue = SetColor Foreground Vivid Blue
fg_bmagenta = SetColor Foreground Vivid Magenta
fg_bcyan = SetColor Foreground Vivid Cyan
fg_bwhite = SetColor Foreground Vivid White

fg_black = SetColor Foreground Dull Black
fg_red = SetColor Foreground Dull Red
fg_green = SetColor Foreground Dull Green
fg_yellow = SetColor Foreground Dull Yellow
fg_blue = SetColor Foreground Dull Blue
fg_magenta = SetColor Foreground Dull Magenta
fg_cyan = SetColor Foreground Dull Cyan
fg_white = SetColor Foreground Dull White

bg_bblack = SetColor Background Vivid Black
bg_bred = SetColor Background Vivid Red
bg_bgreen = SetColor Background Vivid Green
bg_byellow = SetColor Background Vivid Yellow
bg_bblue = SetColor Background Vivid Blue
bg_bmagenta = SetColor Background Vivid Magenta
bg_bcyan = SetColor Background Vivid Cyan
bg_bwhite = SetColor Background Vivid White

bg_black = SetColor Background Dull Black
bg_red = SetColor Background Dull Red
bg_green = SetColor Background Dull Green
bg_yellow = SetColor Background Dull Yellow
bg_blue = SetColor Background Dull Blue
bg_magenta = SetColor Background Dull Magenta
bg_cyan = SetColor Background Dull Cyan
bg_white = SetColor Background Dull White
