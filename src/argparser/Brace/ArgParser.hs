module Brace.ArgParser (Parser.parse, tests) where

import Test.HUnit

import qualified Brace.ArgParser.Parser as Parser (parse, tests)

tests :: Test
tests = Parser.tests
