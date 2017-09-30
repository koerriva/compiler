module Main where

import Lang.Parser
import Lang.Type
import Text.Parsec

main :: IO ()
main = do
  case parse identifier "" "voisd" of
    Left err -> print err
    Right xs -> print xs
  case parse number "" "0xffff" of
      Left err -> print err
      Right xs -> print xs
  case parse number "" "007777" of
        Left err -> print err
        Right xs -> print xs

