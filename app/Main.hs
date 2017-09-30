module Main where

import Lang.Parser
import Lang.Type
import Text.Parsec hiding (string)
import Text.ParserCombinators.Parsec hiding (string)

main :: IO ()
main = do
  test identifier "void"
  test number "0xffff"
  test number "007777"
  test chr "'x'"
  test string "\"test\""

test :: Parser Token -> String -> IO ()
test p s = case parse p "" s of
  Left err -> print err
  Right xs -> print xs