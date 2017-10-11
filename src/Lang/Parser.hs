module Lang.Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import Lang.Lexer
import Lang.Syntax

import Debug.Trace

int :: Parser Expr
int = Int <$> integer

floating :: Parser Expr
floating = Float <$> float

chr :: Parser Expr
chr = Char <$> do
  char '\''
  c <- letter <|> alphaNum
  char '\''
  return c

str :: Parser Expr
str = String <$> do{char '"';s <- many (letter <|> alphaNum);char '"';return s}

binop = Ex.Infix (BinaryOp <$> op) Ex.AssocLeft
unop = Ex.Prefix (UnaryOp <$> op)

binary s = Ex.Infix (reservedOp s >> return (BinaryOp s))

op :: Parser String
op = do
  whitespace
  o <- operator
  whitespace
  return o

binops = [[binary "=" Ex.AssocLeft]
        ,[binary "*" Ex.AssocLeft,binary "/" Ex.AssocLeft]
        ,[binary "+" Ex.AssocLeft,binary "-" Ex.AssocLeft]
        ,[binary "<" Ex.AssocLeft,binary ">" Ex.AssocLeft]]
table = ["+","-","*","/",">","<","="]
binaryOp =
  parens $ do
    name <- op
    f1 <- factor
    f2 <- factor
    return $ BinaryOp name f1 f2
expr :: Parser Expr
--expr =  Ex.buildExpressionParser (binops ++ [[unop],[binop]]) factor
expr = try binaryOp
  <|> call

variable :: Parser Expr
variable = Var <$> identifier

key :: Parser Expr
key = do
  char ':'
  String s <- str
  return $ Key s

signType :: Parser SignType
signType = do
  reservedOp ":"
  reserved "in"
  tin <- brackets $ sepBy (many letter) (char ',' <|> space)
  reservedOp ":"
  reserved "out"
  tout <- many letter
  return $ SignType (map read tin) (read tout)

function :: Parser Expr
function =
  parens $ do
    reserved "defn"
    name <- identifier
    types <- braces signType
    args <- brackets $ many identifier
    body <- many expr
    return $ Function name types args body

extern :: Parser Expr
extern =
  parens $ do
    reserved "ffi"
    name <- identifier
    types <- braces signType
    args <- brackets $ many identifier
    traceShowM args
    return $ Extern name types args

call :: Parser Expr
call =
  parens $ do
    name <- choice [do{reserved "if";return "if"},do{reserved "do";return "do"},identifier]
    args <- many factor
    case name of
      "if" -> return $ If (head args) (args !! 1) (args !! 2)
      "do" -> return $ Do args
      _    -> return $ Call name args

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- factor
--  reserved "then"
  tr <- factor
--  reserved "else"
  fl <- factor
  return $ If cond tr fl

for :: Parser Expr
for = do
  reserved "for"
  var <- identifier
  reservedOp "="
  start <- expr
  reservedOp ","
  cond <- expr
  reservedOp ","
  step <- expr
  reserved "in"
  body <- expr
  return $ For var start cond step body

letins :: Parser Expr
letins = do
  reserved "var"
  defs <- commaSep $ do
    var <- identifier
    reservedOp "="
    val <- expr
    return (var, val)
  reserved "in"
  body <- expr
  return $ foldr (uncurry Let) body defs

unarydef :: Parser Expr
unarydef = do
  reserved "def"
  reserved "unary"
  o <- op
  args <- parens $ many identifier
  body <- expr
  return $ UnaryDef o args body

binarydef :: Parser Expr
binarydef = do
  reserved "def"
  reserved "binary"
  o <- op
  prec <- int
  args <- parens $ many identifier
  body <- expr
  return $ BinaryDef o args body

factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try chr
      <|> try str
      <|> try variable
      <|> try letins
      <|> for
      <|> expr

defn :: Parser Expr
defn = try extern
    <|> function
--    <|> try unarydef
--    <|> try binarydef
--    <|> expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many defn

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents defn) "<eval>"

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel = parse (contents toplevel) "<stdin>"