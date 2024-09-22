{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UHF.Parts.Parser.Grammar
    ( Grammar (..)
    , filter_rules_with_nt

    , Nonterminal (..)
    , Terminal
    , Symbol (..)

    , Rule (..)

    , NTResultTypes
    , ReduceFnMap

    , GrammarMonad
    , GrammarCreationError (..)
    , nt
    , get_nt_ty
    , toplevel
    , rule
    , (-->)
    , ToSymbol (..)
    , prod_join

    , make_grammar

    ) where

import UHF.Prelude

import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH.Syntax

import qualified UHF.Data.Token as Token

data Grammar = Grammar {augment_rule :: Rule, all_rules :: [Rule], nt_result_types :: NTResultTypes, reduce_fn_map :: ReduceFnMap}

type NTResultTypes = Map Nonterminal (TH.Q TH.Type)
data Nonterminal = Nonterminal Text | Augment deriving (Show, Eq, Ord, TH.Syntax.Lift)

type Terminal = Token.TokenType

data Symbol = S'T Terminal | S'NT Nonterminal deriving (Show, Eq, Ord)

instance Format Nonterminal where
    format (Nonterminal t) = t
    format Augment = "AUGMENT"
instance Format Symbol where
    format (S'T t) = format t
    format (S'NT nt) = format nt

type ReduceFnMap = Map Rule (TH.Q TH.Exp)
data Rule = Rule Integer Nonterminal [Symbol] deriving (Show, Eq, Ord)

newtype GrammarMonad a = GrammarMonad (State (GrammarMonadState) a) deriving (Functor, Applicative, Monad)
data GrammarMonadState = GrammarMonadState
    { gms_toplevel :: (Maybe Nonterminal)
    , gms_nt_result_tys :: NTResultTypes
    , gms_reduce_fn_map :: ReduceFnMap
    , gms_rules :: [Rule]
    , gms_cur_rule_number :: Integer
    }

nt :: Text -> TH.Q TH.Type -> GrammarMonad Nonterminal
nt t ty = do
    let nt = Nonterminal t
    GrammarMonad $ state $ (\gms -> ((), gms {gms_nt_result_tys = Map.insert nt ty (gms_nt_result_tys gms)}))
    pure $ Nonterminal t

get_nt_ty :: Nonterminal -> GrammarMonad (TH.Q TH.Type)
get_nt_ty nt = do
    gms <- GrammarMonad get
    pure (gms_nt_result_tys gms Map.! nt)

toplevel :: Nonterminal -> GrammarMonad Nonterminal
toplevel nt = GrammarMonad $ State.state $ \gms -> (nt, gms {gms_toplevel = Just nt})

rule :: Nonterminal -> [Symbol] -> TH.Q TH.Exp -> GrammarMonad ()
rule nt production create_ast =
    GrammarMonad $
        State.state $
            \gms ->
                let rule_n = gms_cur_rule_number gms
                    rule = Rule rule_n nt production
                in ( ()
                   , gms {gms_reduce_fn_map = Map.insert rule create_ast (gms_reduce_fn_map gms), gms_rules = rule : (gms_rules gms), gms_cur_rule_number = rule_n + 1}
                   )

(-->) :: Nonterminal -> ([Symbol], TH.Q TH.Exp) -> GrammarMonad ()
(-->) nt (prod, create_ast) = rule nt prod create_ast
infix 2 -->

class ToSymbol t where
    to_symbol :: t -> [Symbol]
instance ToSymbol Symbol where
    to_symbol s = [s]
instance ToSymbol [Symbol] where
    to_symbol = identity
instance ToSymbol Terminal where
    to_symbol t = [S'T t]
instance ToSymbol Nonterminal where
    to_symbol nt = [S'NT nt]
prod_join :: (ToSymbol a, ToSymbol b) => a -> b -> [Symbol]
prod_join a b = to_symbol a ++ to_symbol b

data GrammarCreationError = NoToplevelNonterminal deriving Show
make_grammar :: GrammarMonad () -> Either GrammarCreationError (Grammar)
make_grammar (GrammarMonad g) =
    let (GrammarMonadState toplevel_nonterminal ntrtys rfm rules _) = State.execState g (GrammarMonadState Nothing Map.empty Map.empty [] 1)
    in case toplevel_nonterminal of
        Just toplevel ->
            let augment_rule = Rule 0 Augment [S'NT toplevel]
            in Right $ Grammar augment_rule (augment_rule : rules) ntrtys rfm
        Nothing -> Left NoToplevelNonterminal

filter_rules_with_nt :: Nonterminal -> Grammar -> [Rule]
filter_rules_with_nt nt (Grammar _ rules _ _) = filter (\(Rule _ nt' _) -> nt == nt') rules
