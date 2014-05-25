module Main where

import System.Environment
import System.IO
import System.Exit

import Parser
import AddTypes
import Codegen

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
        Just ops' -> print $ addTypes ops'
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
