{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UHF.PP.IndentationMonad
    ( PP
    , exec_pp

    , first_on_line
    , write
    , indent
    , dedent
    ) where

-- TODO: write tests

import UHF.Prelude hiding (StateT, runStateT, evalStateT, get, modify, Writer, runWriter, tell, execWriter)

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.CPS
import qualified Data.Text as Text

data IndentState = IndentState Int Bool
newtype PP a = PP (StateT IndentState (Writer Text) a) deriving (Functor, Applicative, Monad)

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
            PP $
                lift (tell "\n") >>
                modify (\ (IndentState cur_indent _) -> IndentState cur_indent True)

        put_chunk (Just c)
            | Text.null c = pure ()
            | otherwise = PP $
                get >>= (\case
                    IndentState _ True -> put_indent
                    _ -> pure ()) >>
                modify (\ (IndentState cur_indent _) -> IndentState cur_indent False) >>
                lift (tell c)

        put_indent = get >>= \ (IndentState indent _) -> lift (tell $ Text.replicate (indent * 4) (Text.singleton ' '))

indent :: PP ()
indent = PP $ modify $ \ (IndentState indent at_start) -> IndentState (indent + 1) at_start
dedent :: PP ()
dedent = PP $ modify $ \ (IndentState indent at_start) -> IndentState (indent - 1) at_start
