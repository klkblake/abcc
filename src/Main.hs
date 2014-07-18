module Main where

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
        Just ops' -> case runStateT (raise (addTypes ops') >>= unifyTypes >>= resolveFlags >>= mapM reifyPTyped) emptyTCX of
                         Left err -> putStrLn err >> exitFailure
                         Right (ops'', tcx) -> putStrLn (showOps 0 ops'') >> print tcx
        Nothing -> exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
        []          -> doCompile
        ["compile"] -> doCompile
        ["typecheck"] -> doTypeCheck
        _ -> do putStrLn "Unrecognised arguments"
                exitFailure
