module Brace.ArgParser.Description
    ( Description(..)

    , HelpMessage(..)
    , ArgProps(..)

    , ShortFlagName(..)
    , LongFlagName(..)
    , FlagProps(..)

    , ValueName(..)
    , NValues(..)
    , TakesValueProps(..)

    , Arg(..)

    , positional
    , flag
    , option

    , get_flag
    , get_positionals
    ) where

import Data.List
import Data.Maybe

data Description = Description [Arg]

newtype HelpMessage = HelpMessage String
data ArgProps = ArgProps HelpMessage

newtype ShortFlagName = ShortFlagName Char
newtype LongFlagName = LongFlagName String
data FlagProps = FlagProps ShortFlagName (Maybe LongFlagName)

newtype ValueName = ValueName String
data NValues = Number Int | ZeroOrMore | OneOrMore
data TakesValueProps = TakesValueProps ValueName NValues

data Arg
    = Positional ArgProps TakesValueProps
    | Flag FlagProps ArgProps (Maybe TakesValueProps)

positional :: String -> String -> NValues -> Arg
positional help_message value_name n_values = Positional (ArgProps $ HelpMessage help_message) (TakesValueProps (ValueName value_name) n_values)

flag :: Char -> Maybe String -> String -> Arg
flag flag_ch flag_name help_message = Flag (FlagProps (ShortFlagName flag_ch) (LongFlagName <$> flag_name)) (ArgProps $ HelpMessage help_message) Nothing

option :: Char -> Maybe String -> String -> String -> NValues -> Arg
option option_ch option_name help_message value_name n_values = Flag (FlagProps (ShortFlagName option_ch) (LongFlagName <$> option_name)) (ArgProps $ HelpMessage help_message) (Just $ TakesValueProps (ValueName value_name) n_values)

get_flag :: Either Char String -> Description -> Maybe (FlagProps, ArgProps, Maybe TakesValueProps)
get_flag name (Description args) =
    let predicate =
            case name of
                Left ch ->
                    let check (Flag (FlagProps (ShortFlagName ch') _) _ _) = ch' == ch
                        check _ = False
                    in check

                Right str ->
                    let check (Flag (FlagProps _ (Just (LongFlagName str'))) _ _) = str' == str
                        check _ = False
                    in check

    in case find predicate args of
        Just (Flag fp ap m_tvp) -> Just (fp, ap, m_tvp)
        _ -> Nothing

get_positionals :: Description -> [(ArgProps, TakesValueProps)]
get_positionals (Description args) =
    let p (Positional ap tvp) = Just (ap, tvp)
        p _ = Nothing
    in mapMaybe p args
