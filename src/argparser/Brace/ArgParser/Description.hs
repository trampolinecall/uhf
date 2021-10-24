module Brace.ArgParser.Description
    ( Description(..)

    , Arg(..)

    , NValues(..)

    , positional
    , flag
    , option
    ) where

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
