module Main where

import Control.Monad
import Control.Monad.Morph
import Control.Monad.ST
import Data.Maybe
import Multiarg hiding (parse, mode, Mode)
import Pipes

import System.Exit
import System.IO
import System.Process

import GraphViz

import Parser
import PeepholePre
import TypeInferencer
import Codegen

helpText :: String -> String
helpText prog = unlines [ prog ++ " - A compiler for Awelon Bytecode"
                        , ""
                        , "Usage: " ++ prog ++ " [--typecheck] [--dump-<stage>[=FILE]]"
                        , ""
                        , "Options:"
                        , "\t--typecheck              Run the typechecker instead of the compiler"
                        , "\t--peephole-pre           Run the pre-typechecker peephole optimiser"
                        , "\t--dump-<stage>[=FILE]    Dump the internal state of the typechecker at a particular stage to a GraphViz file"
                        , "\t\tStages:"
                        , "\t\t  initial        After the opcodes have had types assigned to them"
                        , "\t\t  unified        After the inputs and outputs of adjacent opcodes have been unified"
                        , "\t\t  resolved       After the substructural attributes have been resolved"
                        , "\t\t  substituted    After the variables have been eliminated from the terms"
                        , "\t--dump-all               Equivalent to specifying all of the above --dump-<stage> flags"
                        , "\t--verbose-graphs         Output graphs that more accurately reflect the type inferencer state"
                        ]

data Flag = TypeCheck
          | PeepholePre
          | Dump TIStage (Maybe FilePath)
          | DumpAll
          | VerboseGraphs
          deriving (Eq, Show)

flagSpecs :: [OptSpec Flag]
flagSpecs =
    [ OptSpec ["typecheck"]        [] (NoArg TypeCheck)
    , OptSpec ["peephole-pre"]     [] (NoArg PeepholePre)
    , OptSpec ["dump-initial"]     [] . OptionalArg $ return . Dump TIInitial
    , OptSpec ["dump-unified"]     [] . OptionalArg $ return . Dump TIUnified
    , OptSpec ["dump-resolved"]    [] . OptionalArg $ return . Dump TIResolved
    , OptSpec ["dump-substituted"] [] . OptionalArg $ return . Dump TISubstituted
    , OptSpec ["dump-all"]         [] $ NoArg DumpAll
    , OptSpec ["verbose-graphs"]   [] $ NoArg VerboseGraphs
    ]

data Options = Options { optDump :: [(TIStage, String)]
                       , optGraphMode :: Mode
                       }

processFlags :: [Flag] -> Options
processFlags flags =
     let fs = if DumpAll `elem` flags
                  then ($ flags) . foldr1 (.) $ map addIfMissing [minBound .. maxBound]
                  else flags
         mode = if VerboseGraphs `elem` flags
                    then Verbose
                    else Compact
     in Options (mapMaybe processDump fs) mode
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

doPeepholePre :: IO ()
doPeepholePre = do
    input <- getContents
    ops <- parse input
    case fmap peepholePre ops of
        Just ops' -> mapM_ (putStr . show) ops' >> putStrLn ""
        Nothing   -> exitFailure

doTypeCheck :: Options -> IO ()
doTypeCheck opts = do
    input <- getContents
    ops <- parse input
    case ops of
        Just ops' -> do
            let ops'' = peepholePre ops'
            let dump = map fst $ optDump opts
            res <- runEffect (for (hoist stToIO $ inferTypes (optGraphMode opts) dump ops'') $ lift . writeGraph)
            case res of
                Left (i, graph) -> do
                    hPutStrLn stderr $ "# Failed while unifying index " ++ show i
                    hPutStrLn stderr   "# Run this error though dot (from the GraphViz package) for more info"
                    hPutStrLn stderr graph
                    isTerm <- hIsTerminalDevice stderr
                    when isTerm $ do
                        (Just hin, _, _, ph) <- createProcess (proc "dot" ["-Tx11"]){ std_in = CreatePipe }
                        hPutStrLn hin graph
                        hClose hin
                        _ <- waitForProcess ph
                        return ()
                    exitFailure
                Right exprs -> mapM_ print exprs
        Nothing   -> exitFailure
  where
    writeGraph (stage, graph) = writeFile (fromJust . lookup stage $ optDump opts) graph

main :: IO ()
main = do
    flags <- simpleHelp helpText flagSpecs Intersperse . const . Left $ ErrorMsg "Positional arguments are not allowed"
    if PeepholePre `elem` flags
        then doPeepholePre
        else
            if TypeCheck `elem` flags
                then doTypeCheck $ processFlags flags
                else doCompile
