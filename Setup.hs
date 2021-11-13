import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.Utils

import Distribution.Types.BuildInfo
import Distribution.Types.LocalBuildInfo
import Distribution.Types.ComponentLocalBuildInfo

import Data.List.Extra (dropPrefix, dropSuffix)

main =
    let hooks =
            simpleUserHooks
            { hookedPreProcessors =
                ("test", test_preprocessor) : hookedPreProcessors simpleUserHooks
            }
    in defaultMainWithHooks hooks

test_preprocessor :: BuildInfo  -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
test_preprocessor build local clocal =
    PreProcessor
    { platformIndependent = True
    , runPreProcessor =
        mkSimplePreProcessor $ \ infile outfile verbosity ->
            let package_name = dropSuffix ".test" $ dropPrefix "test/" infile
                package_dir = "src/" ++ package_name ++ "/"

            in getDirectoryContentsRecursive package_dir >>= \ modules ->
            let module_names = map (dropSuffix ".hs" . map (\ c -> if c == '/' then '.' else c)) modules

                test_contents =
                    "module Main where\n\
                    \\n\
                    \import Test.HUnit\n\
                    \\n" ++

                    concatMap (\ mn -> "import qualified " ++ mn ++ " (tests)\n") module_names ++

                    "\n\
                    \main :: IO ()\n\
                    \main = runTestTTAndExit $ test [" ++
                        intercalate ", " (map (\ mn -> show mn ++ " ~: " ++ mn ++ ".tests") module_names) ++
                        "]\n"

            in writeFile outfile test_contents
    }

