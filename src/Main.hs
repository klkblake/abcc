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
        Just ops' -> print $ runStateT (raise (addTypes ops') >>= unifyTypes >>= resolveFlags >>= mapM reifyTyped) emptyTCX
        Nothing -> exitFailure
  where
    reifyTyped (Typed a b (LitBlock ops)) = Typed <$> reify a <*> reify b <*> (LitBlock <$> mapM reifyTyped ops)
    reifyTyped (Typed a b op) = Typed <$> reify a <*> reify b <*> return op

main :: IO ()
main = do
    args <- getArgs
    case args of
        []          -> doCompile
        ["compile"] -> doCompile
        ["typecheck"] -> doTypeCheck
        _ -> do putStrLn "Unrecognised arguments"
                exitFailure
