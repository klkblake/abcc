module Main where

import Control.Monad.State

import System.Environment
import System.IO
import System.Exit

import Parser
import TypeInferencer
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
        Just ops' -> putStrLn $ inferTypes ops'
        Nothing   -> exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
        []          -> doCompile
        ["compile"] -> doCompile
        ["typecheck"] -> doTypeCheck
        _ -> do putStrLn "Unrecognised arguments"
                exitFailure
