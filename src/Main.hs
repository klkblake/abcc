module Main where

import Control.Monad.Morph
import Control.Monad.ST
import Data.Maybe
import Multiarg hiding (parse)
import Pipes

import System.IO
import System.Exit

import Parser
import TypeInferencer
import Codegen

helpText :: String -> String
helpText prog = unlines [ prog ++ " - A compiler for Awelon Bytecode"
                        , ""
                        , "Usage: " ++ prog ++ " [--typecheck] [--dump-<stage>[=FILE]]"
                        , ""
                        , "Options:"
                        , "\t--typecheck              Run the typechecker instead of the compiler"
                        , "\t--dump-<stage>[=FILE]    Dump the internal state of the typechecker at a particular stage to a GraphViz file"
                        , "\t\tStages:"
                        , "\t\tinitial        After the opcodes have had types assigned to them"
                        , "\t\tunified        After the inputs and outputs of adjacent opcodes have been unified"
                        , "\t\tresolved       After the substructural attributes have been resolved"
                        , "\t\tsubstituted    After the variables have been eliminated from the terms"
                        ]

data Flag = TypeCheck
          | Dump TIStage (Maybe FilePath)
          | DumpAll
          deriving (Eq, Show)

flagSpecs :: [OptSpec Flag]
flagSpecs =
    [ OptSpec ["typecheck"]        [] (NoArg TypeCheck)
    , OptSpec ["dump-initial"]     [] . OptionalArg $ return . Dump TIInitial
    , OptSpec ["dump-unified"]     [] . OptionalArg $ return . Dump TIUnified
    , OptSpec ["dump-resolved"]    [] . OptionalArg $ return . Dump TIResolved
    , OptSpec ["dump-substituted"] [] . OptionalArg $ return . Dump TISubstituted
    , OptSpec ["dump-all"]         [] $ NoArg DumpAll
    ]

data Options = Options { optDump :: [(TIStage, String)] }
             deriving Show

processFlags :: [Flag] -> Options
processFlags flags =
     let fs = if DumpAll `elem` flags
                  then ($ flags) . foldr1 (.) $ map addIfMissing [minBound .. maxBound]
                  else flags
     in Options $ mapMaybe processDump fs
  where
    addIfMissing stage fs = if any (isDump stage) fs
                                then fs
                                else Dump stage Nothing:fs
    isDump stage (Dump stage' _) = stage == stage'
    isDump _ _ = False
    processDump (Dump stage (Just file)) = Just (stage, file)
    processDump (Dump stage Nothing)     = Just (stage, defaultFile stage)
    processDump _ = Nothing
    defaultFile TIInitial     = "initial.dot"
    defaultFile TIUnified     = "unified.dot"
    defaultFile TIResolved    = "resolved.dot"
    defaultFile TISubstituted = "substituted.dot"

doCompile :: IO ()
doCompile = do
    input <- getContents
    ops <- parse input
    case fmap compile ops of
        Just (Right prog)   -> putStr prog
        Just (Left  errmsg) -> do hPutStrLn stderr $ "Failure: " ++ errmsg
                                  exitFailure
        Nothing -> exitFailure

doTypeCheck :: Options -> IO ()
doTypeCheck opts = do
    input <- getContents
    ops <- parse input
    case ops of
        Just ops' -> do
            let dump = map fst $ optDump opts
            res <- runEffect (for (hoist stToIO $ inferTypes dump ops') $ lift . writeGraph)
            case res of
                Left (i, graph) -> do
                    hPutStrLn stderr $ "# Failed while unifying index " ++ show i
                    hPutStrLn stderr $ "# Run this error though dot (from the GraphViz package) for more info"
                    hPutStrLn stderr graph
                    exitFailure
                Right exprs -> mapM_ print exprs
        Nothing   -> exitFailure
  where
    writeGraph (stage, graph) = writeFile (fromJust . lookup stage $ optDump opts) graph

main :: IO ()
main = do
    flags <- simpleHelp helpText flagSpecs Intersperse . const . Left $ ErrorMsg "Positional arguments are not allowed"
    if TypeCheck `elem` flags
        then doTypeCheck $ processFlags flags
        else doCompile
