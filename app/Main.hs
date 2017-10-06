module Main where

import Lang.Parser
import Lang.Codegen
import Lang.Emit
import Lang.Syntax

import Control.Monad (void)
import Control.Monad.Trans
import System.IO
import System.Environment
import System.Console.Haskeline

import qualified LLVM.AST as AST

initModule :: AST.Module
initModule = emptyModule "default model"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = parseToplevel source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      ast <- codegen modo ex
      return $ Just ast

process' :: String -> IO (Maybe [Expr])
process' source = do
  let res = parseToplevel source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      print ex
      return (Just ex)

processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

processFile' :: String -> IO (Maybe [Expr])
processFile' fname = readFile fname >>= process'

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
  loop mod = do
    minput <- getInputLine "lisp 1.0> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        modn <- liftIO $ process mod input
        case modn of
          Just modn -> loop modn
          Nothing -> loop mod

repl' :: IO ()
repl' = runInputT defaultSettings (loop [])
  where
    loop ls = do
      line <- getInputLine "lisp>"
      case line of
        Nothing -> outputStrLn "Bye."
        Just input -> do
          expr <- liftIO $ process' input
          case expr of
            Just exprs -> loop exprs
            Nothing -> loop ls

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    [fname] -> void (processFile fname)