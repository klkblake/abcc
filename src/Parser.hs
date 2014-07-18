module Parser (parse) where

import Control.Applicative
import Data.Functor.Identity
import Data.List
import Data.Maybe
import Data.Monoid
import System.IO

import qualified Text.PrettyPrint.ANSI.Leijen as Pretty
import Text.Trifecta
import Text.Trifecta.Delta

import Op

opcodes :: [(Char, UntypedOp)]
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
          , ('k', Relevant)
          , ('f', Affine)
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

parseOp :: Parser UntypedOp
parseOp = do
    op <- anyChar
    maybe (unexpected $ show op) return $ lookup op opcodes

parseCap :: Parser UntypedOp
parseCap = between (char '{') (char '}') $ char ':' *> parseSealer
                                       <|> char '.' *> parseUnsealer
                                       <|> char '&' *> parseAnnotation 
  where
    parseSealer = Sealer <$> parseCapText
    parseUnsealer = Unsealer <$> parseCapText
    parseAnnotation = do
        cap <- parseCapText
        case cap of
            "≡"                -> return AssertEQ
            "debug print raw"  -> return DebugPrintRaw
            "debug print text" -> return DebugPrintText
            _                  -> fail $ "Unrecognised capability: " ++ show cap
    parseCapText = many $ noneOf "{}\n"

parseText :: Parser String
parseText = between (char '"') (string "\n~") $ intercalate "\n" <$> parseLines
  where
    parseLines = sepBy (many $ notChar '\n') $ string "\n "

parseBlock :: Parser [UntypedOp]
parseBlock = between (char '[') (char ']') parse'

parse' :: Parser [UntypedOp]
parse' = catMaybes <$> many ( oneOf " \n" *> return Nothing
                          <|> Just <$> ((LitBlock . map Identity) <$> parseBlock <|> parseCap <|> LitText <$> parseText <|> try parseOp)
                            )

parse :: String -> IO (Maybe [UntypedOp])
parse input =
    case parseString parse' (Lines 0 0 0 0) input of
        Success a  -> return (Just a)
        Failure xs -> do
            Pretty.displayIO stderr $ Pretty.renderPretty 0.8 80 $ xs <> Pretty.linebreak
            return Nothing
