module UHF.DumpUtils
    ( Dumper
    , run_dumper
    , exec_dumper

    , dump
    , dump_ch
    , indent
    , dedent
    , newline
    ) where

-- TODO: write tests

import UHF.Util.Prelude

import qualified Data.Text as Text

data DumpState = DumpState Int Bool
newtype Dumper a = Dumper { un_dumper :: StateT DumpState (Writer Text) a }

instance Functor Dumper where
    fmap f (Dumper a) = Dumper $ f <$> a

instance Applicative Dumper where
    pure = Dumper . pure
    (Dumper a) <*> (Dumper b) = Dumper $ a <*> b

instance Monad Dumper where
    (Dumper a) >>= f = Dumper $ a >>= un_dumper . f

run_dumper :: Dumper a -> (a, Text)
run_dumper (Dumper d) = runWriter $ evalStateT d (DumpState 0 True)

exec_dumper :: Dumper a -> Text
exec_dumper = snd . run_dumper

dump :: Text -> Dumper ()
dump t = mapM_ dump_ch $ Text.unpack t

output :: Char -> Dumper ()
output = Dumper . lift . tell . Text.singleton

dump_ch :: Char -> Dumper ()
dump_ch '\n' = output '\n' >> Dumper (modify (\ (DumpState cur_indent _) -> DumpState cur_indent True))
dump_ch c =
    Dumper get >>= (\case
        DumpState _ True -> put_indent
        _ -> pure ()) >>
    Dumper (modify $ \ (DumpState cur_indent _) -> DumpState cur_indent False) >>
    output c
    where
        put_indent = Dumper $ get >>= \ (DumpState indent _) -> lift (tell $ Text.replicate indent " ")

indent :: Dumper ()
indent = Dumper $ modify $ \ (DumpState indent at_start) -> DumpState (indent + 4) at_start
dedent :: Dumper ()
dedent = Dumper $ modify $ \ (DumpState indent at_start) -> DumpState (indent - 4) at_start
newline :: Dumper ()
newline = dump_ch '\n'
