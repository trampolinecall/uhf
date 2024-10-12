{-# LANGUAGE TemplateHaskell #-}

module UHF.Data.Token.TH (TokenSpec, tt, generate) where

import UHF.Prelude

import qualified Data.Data as Data
import qualified Data.Dynamic as Dynamic
import Data.String (String)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH.Syntax (Lift)
import UHF.Source.Located (Located (..))

import UHF.Source.EqIgnoringSpans

type TokenSpec = (String, [TH.Q TH.Type], String, TH.Q TH.Exp)

tt :: String -> [TH.Q TH.Type] -> String -> TH.Q TH.Exp -> TokenSpec
tt s fields str_tt str_tok = (s, fields, str_tt, str_tok)

generate :: [TokenSpec] -> TH.Q [TH.Dec]
generate token_specs = do
    token_datatypes_decs <-
        mapM
            ( \(name, field_types, _, _) ->
                TH.dataD
                    (TH.cxt [])
                    (TH.mkName name)
                    []
                    Nothing
                    [TH.normalC (TH.mkName name) (map (fmap (TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness,)) field_types)]
                    [deriving_clause]
            )
            token_specs

    token_dec <-
        TH.dataD
            (TH.cxt [])
            (TH.mkName "Token")
            []
            Nothing
            ( map
                ( \(name, _, _, _) ->
                    TH.normalC (TH.mkName $ "T'" ++ name) [pure (TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, TH.ConT $ TH.mkName name)]
                )
                token_specs
            )
            [deriving_clause]

    token_type_dec <-
        TH.dataD
            (TH.cxt [])
            (TH.mkName "TokenType")
            []
            Nothing
            (map (\(name, _, _, _) -> TH.normalC (TH.mkName $ "TT'" ++ name) []) token_specs)
            [deriving_clause]

    format_token_insts <-
        mapM
            ( \(name, field_types, _, format_t) ->
                TH.instanceD
                    (TH.cxt [])
                    [t|Format $(TH.conT $ TH.mkName name)|]
                    [ TH.funD
                        (TH.mkName "format")
                        [ do
                            field_names <- mapM (\_ -> TH.newName "f") field_types
                            format_t <- format_t
                            TH.clause
                                [TH.conP (TH.mkName name) (map TH.varP field_names)]
                                (TH.normalB $ foldlM (\e a -> [|$(pure e) $(TH.varE a)|]) format_t field_names)
                                []
                        ]
                    ]
            )
            token_specs
    format_token_inst <-
        TH.instanceD
            (TH.cxt [])
            [t|Format $(TH.conT $ TH.mkName "Token")|]
            [ TH.funD
                (TH.mkName "format")
                ( map
                    ( \(name, _, _, _) -> do
                        field_name <- TH.newName "t"
                        TH.clause
                            [TH.conP (TH.mkName $ "T'" ++ name) [TH.varP field_name]]
                            (TH.normalB [|format $(TH.varE field_name)|])
                            []
                    )
                    token_specs
                )
            ]

    format_token_type_inst <-
        TH.instanceD
            (TH.cxt [])
            [t|Format $(TH.conT $ TH.mkName "TokenType")|]
            [ TH.funD
                (TH.mkName "format")
                ( map
                    ( \(name, _, format_tt, _) -> do
                        TH.clause
                            [TH.conP (TH.mkName $ "TT'" ++ name) []]
                            (TH.normalB $ TH.litE $ TH.StringL format_tt)
                            []
                    )
                    token_specs
                )
            ]

    to_token_type_sig <- TH.sigD (TH.mkName "to_token_type") [t|$(TH.conT $ TH.mkName "Token") -> $(TH.conT $ TH.mkName "TokenType")|]
    to_token_type_dec <-
        TH.funD
            (TH.mkName "to_token_type")
            ( map
                ( \(name, _, _, _) ->
                    TH.clause
                        [TH.conP (TH.mkName $ "T'" ++ name) [TH.wildP]]
                        (TH.normalB $ TH.conE $ TH.mkName $ "TT'" ++ name)
                        []
                )
                token_specs
            )

    untoken_sig <- TH.sigD (TH.mkName "untoken") [t|Located $(TH.conT $ TH.mkName "Token") -> Dynamic.Dynamic|]
    untoken_dec <-
        TH.funD
            (TH.mkName "untoken")
            ( map
                ( \(name, _, _, _) -> do
                    sp <- TH.newName "sp"
                    t <- TH.newName "t"
                    TH.clause
                        [[p|Located $(TH.varP sp) $(TH.conP (TH.mkName $ "T'" ++ name) [TH.varP t])|]]
                        (TH.normalB [|Dynamic.toDyn $ Located $(TH.varE sp) $(TH.varE t)|])
                        []
                )
                token_specs
            )

    pure $
        token_datatypes_decs
            ++ [token_dec, token_type_dec]
            ++ format_token_insts
            ++ [format_token_inst, format_token_type_inst, to_token_type_sig, to_token_type_dec, untoken_sig, untoken_dec]
    where
        deriving_clause = TH.derivClause Nothing [[t|Show|], [t|Eq|], [t|Ord|], [t|Generic|], [t|Data.Data|], [t|EqIgnoringSpans|], [t|TH.Syntax.Lift|]]
