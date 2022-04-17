{-# LANGUAGE TemplateHaskell #-}

module UHF.ArgParser.Help
    ( args_help_message

    , tests
    ) where

import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty

import UHF.ArgParser.Description

import Data.List (intercalate)
import Data.Maybe (catMaybes)

takes_value_str :: TakesValueProps -> String
takes_value_str (TakesValueProps (ValueName value_name) n_values) =
    case n_values of
        Number n -> intercalate " " (replicate n value_name)
        ZeroOrMore -> "[" ++ value_name ++ "...]"
        OneOrMore -> value_name ++ "..."

args_usage_message :: Description -> String -> String
args_usage_message desc prog_name =
    let usages =
            (if null (get_flags desc) then [] else ["[flags]"]) ++
            (if null (get_options desc) then [] else ["[options]"]) ++
            map (takes_value_str . snd) (get_positionals desc)

    in "usage: " ++ intercalate " " (prog_name:usages)

args_help_message :: Description -> String -> String
args_help_message desc prog_name =
    let positionals_help =
            let positionals = get_positionals desc
            in if null positionals
                then Nothing
                else Just $ "positional arguments:\n" ++ concat (map positional_help positionals)

        flags_help =
            let flags = get_flags desc
            in if null flags
                then Nothing
                else Just $ "flags:\n" ++ concat (map flag_help flags)

        options_help =
            let options = get_options desc
            in if null options
                then Nothing
                else Just $ "options:\n" ++ concat (map option_help options)

    in intercalate "\n" $
        catMaybes
            [ Just $ args_usage_message desc prog_name ++ "\n"
            , positionals_help
            , flags_help
            , options_help
            ]

arg_help :: String -> String -> String
arg_help arg_desc help_message =
    let help_indent = 24
        help_indent_str = replicate help_indent ' '

        indented_arg_desc = "    " ++ arg_desc

    in if length indented_arg_desc >= help_indent
        then indented_arg_desc ++ "\n" ++ help_indent_str ++ help_message ++ "\n"
        else indented_arg_desc ++ replicate (help_indent - length indented_arg_desc) ' ' ++ help_message ++ "\n"

positional_help :: (ArgProps, TakesValueProps) -> String
positional_help (ArgProps (HelpMessage help_message), TakesValueProps (ValueName value_name) _) =
    arg_help value_name help_message

flag_help :: (FlagProps, ArgProps) -> String
flag_help (FlagProps (ShortFlagName short_name) m_long_name, ArgProps (HelpMessage help_message)) =
    let short_flag_desc = '-' : [short_name]

        flag_desc =
            case m_long_name of
                Nothing -> short_flag_desc
                Just (LongFlagName long_name) -> short_flag_desc ++ ", " ++ "--" ++ long_name

    in arg_help flag_desc help_message

option_help :: (FlagProps, ArgProps, TakesValueProps) -> String
option_help (FlagProps (ShortFlagName short_name) m_long_name, ArgProps (HelpMessage help_message), tvp) =
    let short_flag_desc = '-' : [short_name]

        value_str = takes_value_str tvp

        flag_desc =
            case m_long_name of
                Nothing -> short_flag_desc ++ " " ++ value_str
                Just (LongFlagName long_name) -> short_flag_desc ++ " " ++ value_str ++ ", " ++ "--" ++ long_name ++ " " ++ value_str

    in arg_help flag_desc help_message

case_takes_value_str_single_value :: Assertion
case_takes_value_str_single_value = "POSITIONAL" @=? takes_value_str (TakesValueProps (ValueName "POSITIONAL") (Number 1))
case_takes_value_str_double_value :: Assertion
case_takes_value_str_double_value = "POSITIONAL POSITIONAL" @=? takes_value_str (TakesValueProps (ValueName "POSITIONAL") (Number 2))
case_takes_value_str_one_more_value :: Assertion
case_takes_value_str_one_more_value = "POSITIONAL..." @=? takes_value_str (TakesValueProps (ValueName "POSITIONAL") OneOrMore)
case_takes_value_str_zero_more_value :: Assertion
case_takes_value_str_zero_more_value = "[POSITIONAL...]" @=? takes_value_str (TakesValueProps (ValueName "POSITIONAL") ZeroOrMore)

case_usage_message_empty :: Assertion
case_usage_message_empty = "usage: prog" @=? args_usage_message (Description []) "prog"

case_usage_message_flag :: Assertion
case_usage_message_flag = "usage: prog [flags]" @=? args_usage_message (Description [flag 'c' Nothing ""]) "prog"

case_usage_message_option :: Assertion
case_usage_message_option = "usage: prog [options]" @=? args_usage_message (Description [option 'c' Nothing "" "value_name" (Number 1)]) "prog"

case_usage_message_positional :: Assertion
case_usage_message_positional = "usage: prog POSITIONAL" @=? args_usage_message (Description [positional "" "POSITIONAL" (Number 1)]) "prog"
case_usage_message_positional_multiple :: Assertion
case_usage_message_positional_multiple = "usage: prog POSITIONAL1 POSITIONAL2" @=? args_usage_message (Description [positional "" "POSITIONAL1" (Number 1), positional "" "POSITIONAL2" (Number 1)]) "prog"

case_arg_help :: Assertion
case_arg_help = "    abcde               help\n" @=? arg_help "abcde" "help"
case_arg_help_at_24 :: Assertion
case_arg_help_at_24 =
    "    abcdefghijklmnopqrst\n\
    \                        help\n" @=? arg_help "abcdefghijklmnopqrst" "help"
case_arg_help_more_than_24 :: Assertion
case_arg_help_more_than_24 =
    "    abcdefghijklmnopqrstu\n\
    \                        help\n" @=? arg_help "abcdefghijklmnopqrstu" "help"

case_positional_help :: Assertion
case_positional_help = "    POSITIONAL          help\n" @=? positional_help (ArgProps (HelpMessage "help"), TakesValueProps (ValueName "POSITIONAL") ZeroOrMore)

case_flag_help :: Assertion
case_flag_help = "    -c                  help\n" @=? flag_help (FlagProps (ShortFlagName 'c') Nothing, ArgProps (HelpMessage "help"))
case_flag_help_long :: Assertion
case_flag_help_long = "    -c, --long          help\n" @=? flag_help (FlagProps (ShortFlagName 'c') (Just $ LongFlagName "long"), ArgProps (HelpMessage "help"))

case_option_help :: Assertion
case_option_help = "    -o [OPT...]         help\n" @=? option_help (FlagProps (ShortFlagName 'o') Nothing, ArgProps (HelpMessage "help"), TakesValueProps (ValueName "OPT") ZeroOrMore)
case_option_help_long :: Assertion
case_option_help_long =
    "    -o [OPT...], --option [OPT...]\n\
    \                        help\n" @=? option_help (FlagProps (ShortFlagName 'o') (Just $ LongFlagName "option"), ArgProps (HelpMessage "help"), TakesValueProps (ValueName "OPT") ZeroOrMore)

case_args_help_message_empty :: Assertion
case_args_help_message_empty = "usage: prog\n" @=? args_help_message (Description []) "prog"

case_args_help_message_positional :: Assertion
case_args_help_message_positional =
    "usage: prog POSITIONAL\n\
    \\n\
    \positional arguments:\n\
    \    POSITIONAL          help\n" @=? args_help_message (Description [positional "help" "POSITIONAL" (Number 1)]) "prog"

case_args_help_message_flag :: Assertion
case_args_help_message_flag =
    "usage: prog [flags]\n\
    \\n\
    \flags:\n\
    \    -c                  help\n" @=? args_help_message (Description [flag 'c' Nothing "help"]) "prog"

case_args_help_message_option :: Assertion
case_args_help_message_option =
    "usage: prog [options]\n\
    \\n\
    \options:\n\
    \    -o OPT              help\n" @=? args_help_message (Description [option 'o' Nothing "help" "OPT" (Number 1)]) "prog"

case_args_help_message_all :: Assertion
case_args_help_message_all =
    "usage: prog [flags] [options] POSITIONAL\n\
    \\n\
    \positional arguments:\n\
    \    POSITIONAL          help\n\
    \\n\
    \flags:\n\
    \    -c                  help\n\
    \\n\
    \options:\n\
    \    -o OPT              help\n" @=? args_help_message (Description [positional "help" "POSITIONAL" (Number 1), flag 'c' Nothing "help", option 'o' Nothing "help" "OPT" (Number 1)]) "prog"

tests :: TestTree
tests = $(testGroupGenerator)
