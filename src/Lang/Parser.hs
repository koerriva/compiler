module Lang.Parser where

import Lang.Type
import Text.ParserCombinators.Parsec

identifier :: Parser Token
identifier = do
  a <- letter <|> char '_'
  b <- many (alphaNum <|> char '_' )
  let name = a:b
  return $ Id name

number :: Parser Token
number = do
  a <- digit
  case a of
    '0' -> do
      b <- alphaNum
      case b of
        'x' -> do
          c <- many hexDigit
          return $ Num (a : b : c)
        'b' -> do
          c <- many (oneOf "01")
          return $ Num (a : b : c)
        _ -> do
          o <- lookAhead digit
          c <- many octDigit
          return $ Num (a:o:c)
    _ -> do
      b <- many digit
      return $ Num (a:b)

string :: Parser Token
string = do
  a <- between (char '"') (char '"') $ many letter
  return $ Str a

chr :: Parser Token
chr = do
  a <- between (char '\'') (char '\'') letter
  return $ Char a