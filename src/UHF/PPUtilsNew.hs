{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- eventually will replace the old one
module UHF.PPUtilsNew
    ( Consistency(..)
    , Token(..)
    , render

    , flat_block
    , indented_block
    , braced_block
    , braced_comma_list
    , bracketed_comma_list
    , parenthesized_comma_list
    , comma_separated
    ) where

import UHF.Util.Prelude

import qualified UHF.PPUtils as PPUtilsOld

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
render = PPUtilsOld.exec_pp . render'
    where
        render' (Block consistency Nothing left_if_single_line right_if_single_line items) = render_block True consistency "" " " left_if_single_line right_if_single_line items
        render' (Block consistency (Just delim) left_if_single_line right_if_single_line items) = render_block True consistency delim (delim <> " ") left_if_single_line right_if_single_line items
        render' (NoIndentBlock consistency Nothing left_if_single_line right_if_single_line items) = render_block False consistency "" " " left_if_single_line right_if_single_line items
        render' (NoIndentBlock consistency (Just delim) left_if_single_line right_if_single_line items) = render_block False consistency delim (delim <> " ") left_if_single_line right_if_single_line items
        render' (FirstOnLineIfMultiline tok) =
            let tok' = render' tok
            in PPUtilsOld.first_on_line >>= \ first_on_line ->
            if PPUtilsOld.is_multiline tok' && not first_on_line
                  then PPUtilsOld.write "\n" >> PPUtilsOld.indent >> tok' >> PPUtilsOld.dedent
                  else tok'
        render' (List items) = mapM_ render' items
        render' (String s) = PPUtilsOld.write s

        -- TODO: break consistency; right now everything is treated as consistent breaking
        render_block needs_indent consistency delim_if_broken delim_if_single_line left_if_single_line right_if_single_line items =
            let items' = map render' items
                any_multiline = any PPUtilsOld.is_multiline items'
            in if any_multiline || length items > 1 -- needs to be broken
                then
                    (if needs_indent
                        then PPUtilsOld.write "\n" >> PPUtilsOld.indent
                        else pure ()) >>
                    mapM (>> PPUtilsOld.write (delim_if_broken <> "\n")) items' >>
                    (if needs_indent
                        then PPUtilsOld.dedent
                        else pure ())
                else
                    if null items'
                       then pure ()
                       else
                            PPUtilsOld.write (fromMaybe "" left_if_single_line) >>
                            intercalate_delim True items' >> -- true if first
                            PPUtilsOld.write (fromMaybe "" right_if_single_line)
            where
                intercalate_delim _ [] = pure ()
                intercalate_delim True (x:more) = x >> intercalate_delim False more
                intercalate_delim False (x:more) = PPUtilsOld.write delim_if_single_line >> x >> intercalate_delim False more

flat_block :: [Token] -> Token
flat_block = NoIndentBlock Consistent Nothing Nothing Nothing

indented_block :: [Token] -> Token
indented_block = Block Consistent Nothing Nothing Nothing

braced_block :: [Token] -> Token
braced_block items = List ["{", Block Consistent Nothing (Just " ") (Just " ") items, "}"]

braced_comma_list :: [Token] -> Token
braced_comma_list items = List ["{", Block Consistent (Just ",") (Just " ") (Just " ") items, "}"]

bracketed_comma_list :: [Token] -> Token
bracketed_comma_list items = List ["[", Block Consistent (Just ",") Nothing Nothing items, "]"]

parenthesized_comma_list :: [Token] -> Token
parenthesized_comma_list items = List ["(", Block Inconsistent (Just ",") Nothing Nothing items, ")"]

comma_separated :: Consistency -> [Token] -> Token
comma_separated consistency = Block consistency (Just ",") Nothing Nothing
