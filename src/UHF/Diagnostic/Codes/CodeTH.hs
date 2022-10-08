module UHF.Diagnostic.Codes.CodeTH where

import UHF.Util.Prelude

import qualified Data.Text as Text
import qualified Language.Haskell.TH as TH
import qualified UHF.Diagnostic.Codes.Code as Code

import Data.String (String)

code :: Code.Type -> Int -> String -> TH.Q [TH.Dec]
code ty num var =
    let diag_code =
            let ty_letter = case ty of
                    Code.Error -> "E"
                    Code.Warning -> "W"
                    Code.DebugMessage -> "DB"
                    Code.InternalError -> "IE"

                num_str = show num

            in ty_letter ++ replicate (4 - length num_str) '0' ++ num_str

        var_name = TH.mkName var
        var_name_p = pure $ TH.VarP var_name

        code_var_name = TH.mkName $ "code_" ++ diag_code
        code_var_name_p = pure $ TH.VarP code_var_name

        diag_name = map (\ ch -> if ch == '_' then '-' else ch) var
        diag_code_lit = pure $ TH.LitE $ TH.StringL diag_code
        name_lit = pure $ TH.LitE $ TH.StringL diag_name
    in
    [d|
        $code_var_name_p = Code.Code ty (Just (Text.pack $diag_code_lit, Text.pack $name_lit))
        $var_name_p = $(pure $ TH.VarE code_var_name)
    |] >>= \ ds ->
    [t| Code.Code |] >>= \ code_ty ->
    pure (
        [ TH.SigD code_var_name code_ty
        , TH.SigD var_name code_ty
        ] ++ ds
    )
