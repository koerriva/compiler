module Lang.Syntax where

import Text.Read
import Text.ParserCombinators.ReadPrec
import Debug.Trace


type Name = String

data Expr
  = Int Integer
  | Float Double
  | Char Char
  | String String
  | Key Name
  | Map [(Expr,Expr)]
  | Vector [Expr]
  | Var String
  | Call Name [Expr]
  | Do [Expr]
  | Function Name SignType [Name] [Expr]
  | Extern Name SignType [Name]
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  | If Expr Expr Expr
  | For Name Expr Expr Expr Expr
  | BinaryDef Name [Name] Expr
  | UnaryDef Name [Name] Expr
  | Let Name Expr Expr
  deriving (Eq, Ord, Show)

data SignType = SignType [Type] Type deriving (Eq,Ord,Show)

data Type
  = TInt
  | TFloat
  | TChar
  | TString
  | TMap [(Type,Type)]
  | TVector
  deriving (Eq, Ord, Show)

instance Read Type where
  readPrec = choice[readInt]
    where
      readInt = do
        a <- look
        skip a
        case a of
          "Int"     -> return TInt
          "Float"   -> return TFloat
          "String"  -> return TString
      skip [] = return ()
      skip (a:ls) = do
        get
        skip ls


