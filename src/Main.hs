module Main where

import Control.Applicative
import Control.Monad.State

import System.Environment
import System.IO
import System.Exit

import Type
import Op

import Parser
import AddTypes
import UnifyTypes
import ResolveFlags
import ResolveTypes
import Codegen

raise :: Monad m => State s a -> StateT s m a
raise = state . runState

doCompile :: IO ()
doCompile = do
    input <- getContents
    ops <- parse input
    case fmap compile ops of
        Just (Right prog)   -> putStr prog
        Just (Left  errmsg) -> do hPutStrLn stderr $ "Failure: " ++ errmsg
                                  exitFailure
        Nothing -> exitFailure

doTypeCheck :: IO ()
doTypeCheck = do
    input <- getContents
    ops <- parse input
    case ops of
        Just ops' -> case runStateT (raise (addTypes ops') >>= unifyTypes >>= resolveFlags >>= resolveTypes >>= mapM reifyPTyped) emptyTCX of
                         Left err -> print err >> exitFailure
                         Right (ops'', tcx) -> printOps 0 ops'' >> print tcx
        Nothing -> exitFailure
  where
    reifyPTyped (PartiallyTyped a b (LitBlock ops)) = Typed <$> reify (Var a) <*> reify (Var b) <*> (LitBlock <$> mapM reifyPTyped ops)
    reifyPTyped (PartiallyTyped a b op) = Typed <$> reify (Var a) <*> reify (Var b) <*> return (castOp op)
    printOps i = mapM_ (printTyped i)
    printTyped i (Typed _ b (LitBlock ops)) = putStrLn  (replicate i ' ' ++ "LitBlock: "    ++ show b) >> printOps (i+4) ops
    printTyped i (Typed _ b op)             = putStrLn $ replicate i ' ' ++ show op ++ ":\t" ++ show b

main :: IO ()
main = do
    args <- getArgs
    case args of
        []          -> doCompile
        ["compile"] -> doCompile
        ["typecheck"] -> doTypeCheck
        _ -> do putStrLn "Unrecognised arguments"
                exitFailure
