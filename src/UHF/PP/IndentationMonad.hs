{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UHF.PP.IndentationMonad
    ( PP
    , exec_pp

    , first_on_line
    , is_multiline
    , write
    , indent
    , dedent
    ) where

-- TODO: write tests

import UHF.Util.Prelude hiding (StateT, runStateT, evalStateT, get, modify, Writer, runWriter, tell, execWriter)

import qualified Data.Text as Text

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.CPS

data IndentState = IndentState Int Bool
newtype PP a = PP (StateT IndentState (Writer Text) a) deriving (Functor, Applicative, Monad)

is_multiline :: PP a -> Bool
is_multiline = Text.any (=='\n') . exec_pp

first_on_line :: PP Bool
first_on_line = PP $ get >>= \ (IndentState _ is_first) -> pure is_first

exec_pp :: PP a -> Text
exec_pp (PP d) = execWriter $ evalStateT d (IndentState 0 True)

write :: Text -> PP ()
write t =
    let chunks = Text.split (=='\n') t
    in mapM_ put_chunk $ intersperse Nothing (map Just chunks)
    where
        put_chunk Nothing =
            PP (lift $ tell "\n") >>
            PP (modify (\ (IndentState cur_indent _) -> IndentState cur_indent True))

        put_chunk (Just c)
            | Text.null c = pure ()
            | otherwise =
                PP get >>= (\case
                    IndentState _ True -> put_indent
                    _ -> pure ()) >>
                PP (modify $ \ (IndentState cur_indent _) -> IndentState cur_indent False) >>
                PP (lift $ tell c)

        put_indent = PP $ get >>= \ (IndentState indent _) -> lift (tell $ Text.replicate indent (Text.singleton ' '))

indent :: PP ()
indent = PP $ modify $ \ (IndentState indent at_start) -> IndentState (indent + 4) at_start
dedent :: PP ()
dedent = PP $ modify $ \ (IndentState indent at_start) -> IndentState (indent - 4) at_start
