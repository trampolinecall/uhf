module UHF.Diagnostic.Report.Colors where

import System.Console.ANSI

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

error, warning, debug_message, note, hint, diag_code, diag_desc :: [SGR]

error = [bold, fg_bred]
warning = [bold, fg_bmagenta]
debug_message = [bold]
note = [bold, fg_bgreen]
hint = [bold, fg_bblue]

file_path_color :: [SGR]
file_path_color = [bold, fg_bcyan]

diag_code = [bold]
diag_desc = [bold]
