module Main where

import System.IO
import System.Exit

import Op
import Codegen

add_abc  = [AssocR, Swap, AssocR, SwapD, Swap, Add, AssocL]
mult_abc = [AssocR, Swap, AssocR, SwapD, Swap, Multiply, AssocL]
dup_abc  = [AssocR, Copy, Swap, SwapD, AssocL, Swap, AssocL]
drop_abc = [AssocR, Drop]

opcodes = [ ('l', AssocL)
          , ('r', AssocR)
          , ('w', Swap)
          , ('z', SwapD)
          , ('v', Intro1)
          , ('c', Elim1)
          , ('%', Drop)
          , ('^', Copy)
          , ('$', Apply)
          , ('o', Compose)
          , ('\'', Quote)
          , ('#', IntroNum)
          , ('0', Digit 0)
          , ('1', Digit 1)
          , ('2', Digit 2)
          , ('3', Digit 3)
          , ('4', Digit 4)
          , ('5', Digit 5)
          , ('6', Digit 6)
          , ('7', Digit 7)
          , ('8', Digit 8)
          , ('9', Digit 9)
          , ('+', Add)
          , ('*', Multiply)
          , ('/', Inverse)
          , ('-', Negate)
          , ('Q', Divmod)
          , ('L', AssocLS)
          , ('R', AssocRS)
          , ('W', SwapS)
          , ('Z', SwapDS)
          , ('V', Intro0)
          , ('C', Elim0)
          , ('?', CondApply)
          , ('D', Distrib)
          , ('F', Factor)
          , ('M', Merge)
          , ('K', Assert)
          , ('>', Greater)
          ]

parseOp :: Char -> Either String Op
parseOp op = maybe (fail $ "Unrecognised opcode: " ++ show op) return $ lookup op opcodes

parseCap :: String -> Either String [Op]
parseCap (':':_) = return []
parseCap ('.':_) = return []
parseCap "&debug print raw" = return [DebugPrintRaw]
parseCap "&debug print text" = return [DebugPrintText]
parseCap cap = fail $ "Unrecognised capability: " ++ show cap

parseText :: String -> Either String (String, String)
parseText ('\n':' ':cs) = do (text, cs') <- parseText cs
                             return ('\n':text, cs')
parseText ('\n':'~':cs) = return ("", cs)
parseText ('\n':c:cs) = fail $ "Invalid character " ++ show c ++ " after newline in text literal"
parseText (c:cs) = do (text, cs') <- parseText cs
                      return (c:text, cs')

-- parse code or block, accumulating in reverse on left
--   return properly ordered result and remaining text

parse' :: [Op] -> String -> Either String ([Op],String)
parse' ps ('k':cs) = parse' ps cs
parse' ps ('f':cs) = parse' ps cs
parse' ps (' ':cs) = parse' ps cs
parse' ps ('\n':cs) = parse' ps cs
parse' ps ('$':'c':']':cs) = return (reverse (ApplyTail:ps), cs)
parse' ps (']':cs) = return (reverse ps, cs)

parse' ps ('[':cs) = do (bops, cs') <- parse' [] cs
                        parse' (LitBlock bops : ps) cs'

parse' ps ('{':cs) = do cap <- parseCap $ takeWhile (/= '}') cs
                        parse' (cap ++ ps) . tail $ dropWhile (/= '}') cs
parse' ps ('"':cs) = do (text, cs') <- parseText cs
                        parse' (LitText text : ps) cs'
parse' ps (c:cs) = do op <- parseOp c
                      parse' (op : ps) cs
parse' ps [] = return (reverse ps,[])

parse :: [Op] -> String -> Either String [Op]
parse ps cs = do
    (ps', cs') <- parse' ps cs
    case cs' of
        ""  -> return ps'
        rem -> fail $ "Unmatched closing bracket. Remaining:\n" ++ rem

main :: IO ()
main = do
    input <- getContents
    case parse [] input >>= compile of
        Right prog -> putStr prog
        Left  err  -> do hPutStrLn stderr $ "Failure: " ++ err
                         exitFailure
