{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UHF.PPUtils
    ( PP
    , run_dumper
    , exec_dumper

    , is_multiline
    , write
    , write_ch
    , indent
    , dedent
    , newline
    ) where

-- TODO: write tests

import UHF.Util.Prelude

import qualified Data.Text as Text

data IndentState = IndentState Int Bool
newtype PP a = PP (StateT IndentState (Writer Text) a) deriving (Functor, Applicative, Monad)

is_multiline :: PP a -> Bool
is_multiline = Text.any (=='\n') . exec_dumper

run_dumper :: PP a -> (a, Text)
run_dumper (PP d) = runWriter $ evalStateT d (IndentState 0 True)

exec_dumper :: PP a -> Text
exec_dumper = snd . run_dumper

write :: Text -> PP ()
write t = mapM_ write_ch $ Text.unpack t

output :: Char -> PP ()
output = PP . lift . tell . Text.singleton

write_ch :: Char -> PP ()
write_ch '\n' = output '\n' >> PP (modify (\ (IndentState cur_indent _) -> IndentState cur_indent True))
write_ch c =
    PP get >>= (\case
        IndentState _ True -> put_indent
        _ -> pure ()) >>
    PP (modify $ \ (IndentState cur_indent _) -> IndentState cur_indent False) >>
    output c
    where
        put_indent = PP $ get >>= \ (IndentState indent _) -> lift (tell $ Text.replicate indent " ")

indent :: PP ()
indent = PP $ modify $ \ (IndentState indent at_start) -> IndentState (indent + 4) at_start
dedent :: PP ()
dedent = PP $ modify $ \ (IndentState indent at_start) -> IndentState (indent - 4) at_start
newline :: PP ()
newline = write_ch '\n'
