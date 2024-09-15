{-# LANGUAGE TemplateHaskell #-}

module UHF.Parts.Parser
    ( parse
    , Error.Error
    ) where

import UHF.Prelude

import qualified Data.InfList as InfList

import qualified UHF.Compiler as Compiler
import qualified UHF.Data.AST as AST
import UHF.Data.Token (BaseToken (..), SingleTypeToken (..))
import qualified UHF.Data.Token as Token
import qualified UHF.Parts.Parser.Error as Error
import qualified UHF.Parts.Parser.Generate as Generate
import UHF.Parts.Parser.Grammar
import UHF.Source.Located (Located (Located))
import qualified UHF.Source.Located as Located

-- TODO: improve parser errors

$( let unwrap_right :: Show a => Either a b -> b
       unwrap_right (Left a) = error $ "unwrap_right on Left " ++ show a
       unwrap_right (Right a) = a
   in Generate.make_parse_fn
        "parse'"
        [t|[AST.Decl]|]
        ( Generate.generate_table $ unwrap_right $ make_grammar $ do
            decl_list <- nt "decl_list" [t|[AST.Decl]|] >>= toplevel
            decl <- nt "decl" [t|AST.Decl|]
            decl_data <- nt "decl_data" [t|AST.Decl|]
            decl_typesyn <- nt "decl_typesyn" [t|AST.Decl|]
            decl_binding <- nt "decl_binding" [t|AST.Decl|]
            data_variant_list <- nt "data_variant_list" [t|[AST.DataVariant]|]
            data_variant <- nt "data_variant" [t|AST.DataVariant|]

            aiden <- nt "alpha_iden" [t|AST.Identifier|]
            rule
                aiden
                [S'T $ AlphaIdentifier ()]
                [|
                    \case
                        (Located sp (AlphaIdentifier n)) -> Located sp n
                        _ -> unreachable
                    |]

            rule decl_list [S'NT decl_list, S'NT decl] [|\dl d -> dl ++ [d]|]
            rule decl_list [] [|[]|]

            rule decl [S'NT decl_data] [|identity|]
            -- rule decl [S'NT decl_typesyn] [|identity|]
            -- rule decl [S'NT decl_binding] [|identity|]

            -- TODO: optional type params
            rule
                decl_data
                [ S'T (SingleTypeToken Data)
                , S'NT aiden
                , S'T $ SingleTypeToken OBrace
                , S'NT data_variant_list
                , S'T $ SingleTypeToken CBrace
                , S'T $ SingleTypeToken Semicolon
                ]
                [|\_ name _ variants _ _ -> AST.Decl'Data name [] variants|]
            rule data_variant_list [S'NT data_variant_list, S'NT data_variant] [|\dvs dv -> dvs ++ [dv]|]
            rule data_variant_list [] [|[]|]

            -- TODO: fields
            rule
                data_variant
                [S'NT aiden, S'T $ SingleTypeToken OParen, S'T $ SingleTypeToken CParen, S'T $ SingleTypeToken Semicolon]
                [|\name _ _ _ -> AST.DataVariant'Anon name []|]

            pure ()
        )
 )

parse :: [Token.LToken] -> Token.LToken -> Compiler.WithDiagnostics (Located [Error.Error]) Void [AST.Decl]
parse toks eof_tok =
    let tok_stream = toks InfList.+++ InfList.repeat eof_tok
    in pure $ parse' tok_stream
