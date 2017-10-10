{-# LANGUAGE OverloadedStrings #-}

module Lang.Emit where

import LLVM.Module
import LLVM.Context

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import qualified Data.ByteString.Char8 as BC8

import Lang.Codegen
import qualified Lang.Syntax as S
import Lang.JIT

import Data.Char
import Debug.Trace

one = cons $ C.Int intSize 1
zero = cons $ C.Int intSize 0
false = zero
true = one

llvmType :: S.Type -> AST.Type
llvmType S.TInt = int
llvmType S.TFloat = double
llvmType S.TChar = char
llvmType S.TString = double
llvmType _         = double

toSig :: S.SignType -> [String] -> ([(AST.Type, AST.Name)],AST.Type)
toSig (S.SignType tin tout) args = (map (\(t,arg) -> (int, AST.Name (l2s arg))) (zip tin args),int)

codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function name (S.SignType tin tout) args body) = define (snd fnargs) name (fst fnargs) bls
  where
    fnargs = toSig (S.SignType tin tout) args
    bls =
      createBlocks $
      execCodegen $ do
        entry <- addBlock (s2l entryBlockName)
        setBlock entry
        addSignType name (map llvmType tin) (llvmType tout)
        forM_ (zip tin args) $ \(t,a) -> do
          var <- alloca (llvmType t)
          store var (local (AST.Name (l2s a)))
          assign a var
        cgen (S.Do body) >>= ret

codegenTop (S.Extern name types args) = external (snd fnargs) name (fst fnargs)
  where fnargs = toSig types args


--codegenTop (S.BinaryDef name args body) =
--  codegenTop $ S.Function ("binary" ++ name) args body
--
--codegenTop (S.UnaryDef name args body) =
--  codegenTop $ S.Function ("unary" ++ name) args body

--codegenTop exp = do
--  define double "main" [] blks
--  where
--    blks = createBlocks $ execCodegen $ do
--      entry <- addBlock (s2l entryBlockName)
--      setBlock entry
--      cgen exp >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp int test

binops = Map.fromList [
      ("+", fadd)
    , ("-", fsub)
    , ("*", fmul)
    , ("/", fdiv)
    , ("<", lt)
  ]

cgen :: S.Expr -> Codegen AST.Operand
cgen (S.UnaryOp op a) = cgen $ S.Call ("unary" ++ op) [a]
cgen (S.Let a b c) = do
  i <- alloca int
  val <- cgen b
  store i val
  assign a i
  cgen c
cgen (S.BinaryOp "=" (S.Var var) val) = do
  a <- getvar var
  cval <- cgen val
  store a cval
  return cval
cgen (S.BinaryOp op a b) =
  case Map.lookup op binops of
    Just f -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> cgen (S.Call ("binary" ++ op) [a,b])
cgen (S.Var x) = getvar x >>= load
cgen (S.Int n) = return $ cons $ C.Int intSize (fromIntegral n)
cgen (S.Float n) = return $ cons $ C.Float (F.Double n)
cgen (S.Char c) = return $ cons $ C.Int charSize ((fromIntegral . digitToInt) c)
cgen (S.String s) = do
  let ls = map (C.Int charSize . fromIntegral . ord) s
  return $ cons $ C.Array (T.IntegerType charSize) ls
cgen (S.Call fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name (l2s fn))) largs
cgen (S.Do calls) = do
  rt <- mapM cgen calls
  return $ last rt
cgen (S.If cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"
  -- %entry
  ------------------
  cond <- cgen cond
  test <- fcmp FP.ONE false cond
  cbr test ifthen ifelse -- Branch based on the condition

  -- if.then
  ------------------
  setBlock ifthen
  trval <- cgen tr       -- Generate code for the true branch
  br ifexit              -- Branch to the merge block
  ifthen <- getBlock

  -- if.else
  ------------------
  setBlock ifelse
  flval <- cgen fl       -- Generate code for the false branch
  br ifexit              -- Branch to the merge block
  ifelse <- getBlock

  -- if.exit
  ------------------
  setBlock ifexit
  phi int [(trval, ifthen), (flval, ifelse)]

cgen (S.For ivar start cond step body) = do
  forloop <- addBlock "for.loop"
  forexit <- addBlock "for.exit"

  -- %entry
  ------------------
  i <- alloca int
  istart <- cgen start           -- Generate loop variable initial value
  stepval <- cgen step           -- Generate loop variable step

  store i istart                 -- Store the loop variable initial value
  assign ivar i                  -- Assign loop variable to the variable name
  br forloop                     -- Branch to the loop body block

  -- for.loop
  ------------------
  setBlock forloop
  cgen body                      -- Generate the loop body
  ival <- load i                 -- Load the current loop iteration
  inext <- fadd ival stepval     -- Increment loop variable
  store i inext

  cond <- cgen cond              -- Generate the loop condition
  test <- fcmp FP.ONE false cond -- Test if the loop condition is True ( 1.0 )
  cbr test forloop forexit       -- Generate the loop condition

  setBlock forexit
  return zero

cgen x = error (show x)
-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen modo fns = do
  let modn = mapM codegenTop fns
      ast  = runLLVM modo modn
  runJIT ast
  return ast