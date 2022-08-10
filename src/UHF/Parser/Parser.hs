module UHF.Parser.Parser where

import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty

import qualified UHF.Parser.ParseError as ParseError

import qualified UHF.IO.Location as Location

import qualified UHF.Token as Token

import qualified UHF.Util.InfList as InfList

import qualified Data.Data as Data

type TokenStream = InfList.InfList Token.LToken
type TokenPredicate = Token.Token -> Bool
type BreakingError = ParseError.ParseError
type RecoverableErrors = [ParseError.ParseError]

-- TODO: also clean up this too
-- TODO: allow each thing to provide a custom error function

data Parser r =
    Parser { run_parser :: (TokenStream -> ParseResult (r, TokenStream)) }

instance Functor Parser where
    fmap f (Parser parser) =
        Parser $ \ toks ->
            let res = parser toks
            in (\ (a, toks') -> (f a, toks')) <$> res

instance Applicative Parser where
    pure a = Parser $ \ toks -> pure (a, toks)

    (Parser parser_f) <*> (Parser parser_v) = Parser $ \ toks ->
        -- this is actually in the ParseResult monad and not in the Parser monad
        parser_f toks >>= \ (f, toks') ->
        parser_v toks' >>= \ (v, toks'') ->
        Success (f v, toks'')

instance Monad Parser where
    (Parser parser_a) >>= b = Parser $ \ toks ->
        parser_a toks >>= \ (a, toks') ->
        run_parser (b a) toks'

data ParseResult r
    -- TODO: synchronization predicate
    = Failed RecoverableErrors BreakingError -- (Maybe TokenPredicate)
    | Recoverable RecoverableErrors r
    | Success r
    deriving (Show, Eq)

{-
instance (Eq r) => Eq (ParseResult r) where
    (Failed es1 e1 {- _ -}) == (Failed es2 e2 {- _ -}) = es1 == es2 && e1 == e2
    (Recoverable e1 r1) == (Recoverable e2 r2) = e1 == e2 && r1 == r2
    (Success r1) == (Success r2) = r1 == r2
    _ == _ = False

instance (Show r) => Show (ParseResult r) where
    show pr =
        case pr of
            (Failed es e {- s -}) -> "Failed " ++ show e -- ++ " " ++ show_sync s
            (Recoverable e r) -> "Recoverable " ++ show e ++ " " ++ show r
            (Success r) -> "Success " ++ show r
        where
            -- show_sync (Just _) = "<synchronization predicate>"
            -- show_sync Nothing = "<no synchronization predicate>"
-}

instance Functor ParseResult where
    fmap _ (Failed es e {- sync -}) = Failed es e {- sync -}
    fmap f (Recoverable es r) = Recoverable es (f r)
    fmap f (Success r) = Success (f r)

instance Applicative ParseResult where
    pure = Success

    a <*> b =
        case a of
            Failed a_es a_e {- sync -} -> Failed a_es a_e {- sync -}

            Recoverable a_es a_v ->
                case a_v <$> b of
                    Failed b_es b_e {- sync -} -> Failed (a_es ++ b_es) b_e {- sync -}
                    Recoverable b_es b_v -> Recoverable (a_es ++ b_es) b_v
                    Success b_v -> Recoverable a_es b_v

            Success a_v -> a_v <$> b

instance Monad ParseResult where
    a >>= b =
        case a of
            Failed a_es a_e {- sync -} -> Failed a_es a_e {- sync -}

            Recoverable a_es a_v ->
                case b a_v of
                    Failed b_es b_e {- sync -} -> Failed (a_es ++ b_es) b_e {- sync -}
                    Recoverable b_es b_v -> Recoverable (a_es ++ b_es) b_v
                    Success b_v -> Recoverable a_es b_v

            Success a_v -> b a_v

peek :: Parser Token.LToken
peek = Parser $ \ toks -> Success $ (InfList.head toks, toks)

is_tt :: Token.Token -> Token.Token -> Bool
is_tt a b = Data.toConstr a == Data.toConstr b

consume :: TokenPredicate -> Parser Token.LToken
consume p = Parser $
    \ (tok InfList.::: more_toks) ->
        if p $ Location.unlocate tok
            then Success $ (tok, more_toks)
            else Failed [] $ ParseError.BadToken tok undefined undefined undefined -- TODO

advance :: Parser ()
advance = Parser $ \ toks -> Success ((), InfList.drop1 toks)

return_fail :: RecoverableErrors -> BreakingError -> Parser a
return_fail errs err = Parser $ \ _ -> Failed errs err

return_recoverable :: RecoverableErrors -> a -> Parser a
return_recoverable errs res = Parser $ \ toks -> Recoverable errs (res, toks)

-- combinators 

-- sequence combinator is >>=

choice :: [Parser a] -> Parser a
choice choices = Parser $ \ toks ->
    try_choices choices [] toks
    where
        try_choices (c:cs) breaking_acc toks =
            case run_parser c toks of
                Success r -> return r
                Recoverable es r -> Recoverable es r
                Failed _ err -> try_choices cs (err:breaking_acc) toks
                -- ignore recoverable errors because they only apply if the choice could be decided
                -- e.g. a hypothetical recoverable error like "invalid function name" or something doesn't apply if it can't decide whether or not it is parsing a function or a datatype

        try_choices [] breaking_acc (tok InfList.::: _) = Failed [] (ParseError.NoneMatched tok breaking_acc)

star :: Parser a -> Parser [a]
star a = Parser $ \ toks ->
    star' [] [] toks
    where
        star' err_acc a_acc toks =
            case run_parser a toks of
                Success (r, toks') -> star' err_acc (a_acc ++ [r]) toks'
                Recoverable es (r, toks') -> star' (err_acc ++ es) (a_acc ++ [r]) toks'
                Failed _ _ ->
                -- both errors do not apply if it doesn't work because that just means the list ends there
                    if null err_acc
                        then Success (a_acc, toks)
                        else Recoverable err_acc (a_acc, toks)

plus :: Parser a -> Parser [a]
plus a =
    a >>= \ a_res ->
    star a >>= \ more_as ->
    return (a_res : more_as)

optional :: Parser a -> Parser (Maybe a)
optional a = Parser $ \ toks ->
    case run_parser a toks of
        Success (r, toks') -> Success (Just r, toks')
        Recoverable e (r, toks') -> Recoverable e (Just r, toks')
        Failed _ _ -> Success (Nothing, toks)

-- andpred :: Parser a -> Parser ()
-- notpred :: Parser a -> Parser ()

{- TODO: synchronization (also "synchronization predicate" todo at definition of ParseResult)
sync :: TokenPredicate -> Parser ()
sync sync_p =
    parse_list
        (\case
            Token.EOF -> True
            t -> sync_p t)
        (consume (const $ Just ()) (error "unreachable")) >> return ()
m_sync :: Maybe TokenPredicate -> Parser ()
m_sync Nothing = return ()
m_sync (Just sync_p) = sync sync_p
-}

{-
-- TODO: use NonEmpty.singleton when a new stack resolver comes out with ghc 9.2.2
--  NonEmpty.singleton is only in base-4.15 or higher, but the only lts stack resolver that has that version of base
--  uses ghc 9.0.2, which is currently missing profiling libraries for base
--  this ghc bug is fixed in ghc 9.2.2, but there is not an lts stack resolver that uses ghc 9.2.2 yet
nonempty_singleton :: a -> NonEmpty.NonEmpty a
nonempty_singleton a = a NonEmpty.:| []
-}

is_alpha_iden :: TokenPredicate
is_alpha_iden = is_tt (Token.AlphaIdentifier [])
