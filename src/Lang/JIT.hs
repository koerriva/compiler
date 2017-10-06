{-#LANGUAGE OverloadedStrings #-}
module Lang.JIT where

import LLVM.Module
import LLVM.Context
import LLVM.PassManager
import qualified LLVM.AST as AST
import qualified LLVM.ExecutionEngine as EE
import qualified Data.ByteString.Char8 as BC8
import Foreign.Ptr (FunPtr,castFunPtr)

foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> (IO Double)

run :: FunPtr a -> IO Double
run fn = haskFun (castFunPtr fn :: FunPtr (IO Double))

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

runJIT :: AST.Module -> IO AST.Module
runJIT mod =
  withContext $ \context ->
    jit context $ \executionEngine ->
      withModuleFromAST context mod $ \m ->
        withPassManager passes $ \pm -> do
          runPassManager pm m
          optmod <- moduleAST m
          s <- moduleLLVMAssembly m
          BC8.putStrLn s

          EE.withModuleInEngine executionEngine m $ \ee -> do
            mainfn <- EE.getFunction ee (AST.Name "main")
            case mainfn of
              Just fn -> do
                res <- run fn
                BC8.putStrLn $ BC8.pack $ "Eval:"++show res
              Nothing -> return ()
          return optmod