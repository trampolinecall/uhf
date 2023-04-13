module UHF.PP
    ( Consistency(..)
    , Token(..)
    , render

    , flat_block
    , inconsistent_indented_block
    , indented_block
    , braced_block
    , braced_comma_list
    , bracketed_comma_list
    , parenthesized_comma_list
    , comma_separated
    ) where

import UHF.Util.Prelude

import qualified UHF.PP.IndentationMonad as IndentationMonad

import qualified Data.Text as Text
import Data.String (IsString (..))

-- inspired by concepts from oppen's pretty printer
-- the paper: https://www.cs.tufts.edu/~nr/cs257/archive/derek-oppen/prettyprinting.pdf
-- (though i did not really read the paper; i mostly got the concepts from the explanation on the readme for the prettyplease rust crate: https://github.com/dtolnay/prettyplease#algorithm-notes)
data Consistency = Consistent | Inconsistent
data Token
    = Block Consistency (Maybe Text) (Maybe Text) (Maybe Text) [Token]
    | NoIndentBlock Consistency (Maybe Text) (Maybe Text) (Maybe Text) [Token]
    | FirstOnLineIfMultiline Token
    | List [Token]
    | String Text

instance IsString Token where
    fromString = String . Text.pack

render :: Token -> Text
render = IndentationMonad.exec_pp . render'
    where
        render' (Block consistency Nothing left_if_single_line right_if_single_line items) = render_block True consistency "" " " left_if_single_line right_if_single_line items
        render' (Block consistency (Just delim) left_if_single_line right_if_single_line items) = render_block True consistency delim (delim <> " ") left_if_single_line right_if_single_line items
        render' (NoIndentBlock consistency Nothing left_if_single_line right_if_single_line items) = render_block False consistency "" " " left_if_single_line right_if_single_line items
        render' (NoIndentBlock consistency (Just delim) left_if_single_line right_if_single_line items) = render_block False consistency delim (delim <> " ") left_if_single_line right_if_single_line items
        render' (FirstOnLineIfMultiline tok) =
            let tok' = render' tok
            in IndentationMonad.first_on_line >>= \ first_on_line ->
            if IndentationMonad.is_multiline tok' && not first_on_line
                  then IndentationMonad.write "\n" >> IndentationMonad.indent >> tok' >> IndentationMonad.dedent
                  else tok'
        render' (List items) = mapM_ render' items
        render' (String s) = IndentationMonad.write s

        render_block needs_indent consistency delim_if_broken delim_if_single_line left_if_single_line right_if_single_line items =
            let items' = map render' items
                any_multiline = any IndentationMonad.is_multiline items'
            in case consistency of
                Consistent
                    | any_multiline || length items > 1 -> -- all need to be broken
                        when needs_indent
                            (IndentationMonad.write "\n" >> IndentationMonad.indent) >>
                        mapM (>> IndentationMonad.write (delim_if_broken <> "\n")) items' >>
                        when needs_indent IndentationMonad.dedent

                Inconsistent
                    | any_multiline -> -- some need to be broken
                        when needs_indent
                            (IndentationMonad.write "\n" >> IndentationMonad.indent) >>
                        zipWithM
                            (\ i item ->
                                item >>
                                if IndentationMonad.is_multiline item || i == length items' - 1 -- the last one always breaks
                                    then IndentationMonad.write $ delim_if_broken <> "\n"
                                    else IndentationMonad.write delim_if_single_line) [0..] items' >>
                        when needs_indent IndentationMonad.dedent

                _
                    | null items' -> pure ()
                    | otherwise ->
                        IndentationMonad.write (fromMaybe "" left_if_single_line) >>
                        intercalate_delim True items' >> -- true if first
                        IndentationMonad.write (fromMaybe "" right_if_single_line)
            where
                intercalate_delim _ [] = pure ()
                intercalate_delim True (x:more) = x >> intercalate_delim False more
                intercalate_delim False (x:more) = IndentationMonad.write delim_if_single_line >> x >> intercalate_delim False more

flat_block :: [Token] -> Token
flat_block = NoIndentBlock Consistent Nothing Nothing Nothing

indented_block :: [Token] -> Token
indented_block = Block Consistent Nothing Nothing Nothing

inconsistent_indented_block :: [Token] -> Token
inconsistent_indented_block = Block Inconsistent Nothing Nothing Nothing

braced_block :: [Token] -> Token
braced_block items = List ["{", Block Consistent Nothing (Just " ") (Just " ") items, "}"]

braced_comma_list :: Consistency -> [Token] -> Token
braced_comma_list consistency items = List ["{", Block consistency (Just ",") (Just " ") (Just " ") items, "}"]

bracketed_comma_list :: Consistency -> [Token] -> Token
bracketed_comma_list consistency items = List ["[", Block consistency (Just ",") Nothing Nothing items, "]"]

parenthesized_comma_list :: Consistency -> [Token] -> Token
parenthesized_comma_list consistency items = List ["(", Block consistency (Just ",") Nothing Nothing items, ")"]

comma_separated :: Consistency -> [Token] -> Token
comma_separated consistency = Block consistency (Just ",") Nothing Nothing
