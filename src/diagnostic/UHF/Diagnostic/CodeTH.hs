{-# LANGUAGE TemplateHaskell #-}

module UHF.Diagnostic.CodeTH where

import qualified Data.Text as Text
import qualified Language.Haskell.TH as TH
import qualified UHF.Diagnostic.Code as Code

code :: Code.Type -> Int -> String -> TH.Q [TH.Dec]
code ty num name =
    let diag_code =
            let ty_letter = case ty of
                    Code.Error -> "E"
                    Code.Warning -> "W"
                    Code.DebugMessage -> "DB"
                    Code.InternalError -> "IE"

                num_str = show num

            in ty_letter ++ replicate (4 - length num_str) '0' ++ num_str

        var_name = TH.mkName $ map (\ ch -> if ch == '-' then '_' else ch) name
        code_var_name = TH.mkName $ "code_" ++ diag_code

        code_var_name_p = return $ TH.VarP code_var_name
        var_name_p = return $ TH.VarP var_name

        diag_code_lit = return $ TH.LitE $ TH.StringL diag_code
        name_lit = return $ TH.LitE $ TH.StringL name
    in
    [d|
        $code_var_name_p = Code.Code ty (Just (Text.pack $diag_code_lit, Text.pack $name_lit))
        $var_name_p = $(return $ TH.VarE code_var_name)
    |] >>= \ ds ->
    [t| Code.Code |] >>= \ code_ty ->
    return (
        [ TH.SigD code_var_name code_ty
        , TH.SigD var_name code_ty
        ] ++ ds
    )
