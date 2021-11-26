module UHF.Diagnostic.Colors where

import System.Console.ANSI as ANSI

bold, bg_white, bg_cyan, bg_magenta, bg_blue, bg_yellow, bg_green, bg_red, bg_black, bg_bwhite, bg_bcyan, bg_bmagenta, bg_bblue, bg_byellow, bg_bgreen, bg_bred, bg_bblack, fg_white, fg_cyan, fg_magenta, fg_blue, fg_yellow, fg_green, fg_red, fg_black, fg_bwhite, fg_bcyan, fg_bmagenta, fg_bblue, fg_byellow, fg_bgreen, fg_bred, fg_bblack, reset, normal :: ANSI.SGR

bold = ANSI.SetConsoleIntensity ANSI.BoldIntensity
normal = ANSI.SetConsoleIntensity ANSI.NormalIntensity

reset = ANSI.Reset

fg_bblack = ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black
fg_bred = ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red
fg_bgreen = ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green
fg_byellow = ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow
fg_bblue = ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue
fg_bmagenta = ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Magenta
fg_bcyan = ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Cyan
fg_bwhite = ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White

fg_black = ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Black
fg_red = ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Red
fg_green = ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green
fg_yellow = ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Yellow
fg_blue = ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Blue
fg_magenta = ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Magenta
fg_cyan = ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Cyan
fg_white = ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White

bg_bblack = ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Black
bg_bred = ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Red
bg_bgreen = ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Green
bg_byellow = ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Yellow
bg_bblue = ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Blue
bg_bmagenta = ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Magenta
bg_bcyan = ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Cyan
bg_bwhite = ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.White

bg_black = ANSI.SetColor ANSI.Background ANSI.Dull ANSI.Black
bg_red = ANSI.SetColor ANSI.Background ANSI.Dull ANSI.Red
bg_green = ANSI.SetColor ANSI.Background ANSI.Dull ANSI.Green
bg_yellow = ANSI.SetColor ANSI.Background ANSI.Dull ANSI.Yellow
bg_blue = ANSI.SetColor ANSI.Background ANSI.Dull ANSI.Blue
bg_magenta = ANSI.SetColor ANSI.Background ANSI.Dull ANSI.Magenta
bg_cyan = ANSI.SetColor ANSI.Background ANSI.Dull ANSI.Cyan
bg_white = ANSI.SetColor ANSI.Background ANSI.Dull ANSI.White

file_path, error, warning, debug_message, note, diag_code, diag_desc :: [ANSI.SGR]

file_path = [bold, fg_bcyan]

error = [bold, fg_bred]
warning = [bold, fg_bmagenta]
debug_message = [bold, fg_bgreen]
note = [bold, fg_bblue]

diag_code = [bold]
diag_desc = [bold]
