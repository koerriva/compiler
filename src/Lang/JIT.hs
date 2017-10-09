{-#LANGUAGE OverloadedStrings #-}
module Lang.JIT where

import LLVM.Module
import LLVM.Context
import LLVM.PassManager
import qualified LLVM.AST as AST
import qualified LLVM.ExecutionEngine as EE
import qualified Data.ByteString.Char8 as BC8
import qualified LLVM.Internal.Target as Target
import LLVM.Internal.FFI.Target (createTargetMachine)
import Foreign.Ptr (FunPtr,castFunPtr)
import System.Process

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
          -- LLVM汇编
          s <- moduleLLVMAssembly m
          BC8.putStrLn s
          -- 生成文件
          compileToFile m

          EE.withModuleInEngine executionEngine m $ \ee -> do
            mainfn <- EE.getFunction ee (AST.Name "main")
            case mainfn of
              Just fn -> do
                res <- run fn
                BC8.putStrLn $ BC8.pack $ "Eval:"++show res
              Nothing -> return ()
          return optmod

compileToFile :: Module -> IO ()
compileToFile m = do
  Target.initializeNativeTarget
  Target.initializeAllTargets
  Target.withHostTargetMachine (genObjectFile m)
  where
    genObjectFile :: Module -> Target.TargetMachine -> IO ()
    genObjectFile mod tm = do
      let object = "main.o"
      writeObjectToFile tm (File object) mod



