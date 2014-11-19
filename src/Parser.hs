module Parser (parse) where

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Monoid
import System.IO

import qualified Text.PrettyPrint.ANSI.Leijen as Pretty
import Text.Trifecta
import Text.Trifecta.Delta

import Op

opcodes :: [(Char, FlatOp)]
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

parseOp :: Parser FlatOp
parseOp = do
    op <- anyChar
    maybe (unexpected $ show op) return $ lookup op opcodes

parseCap :: Parser (Maybe FlatOp)
parseCap = between (char '{') (char '}') $ char ':' *> parseSealer
                                       <|> char '.' *> parseUnsealer
                                       <|> char '&' *> parseAnnotation 
  where
    parseSealer = Just . Sealer <$> parseCapText
    parseUnsealer = Just . Unsealer <$> parseCapText
    parseAnnotation = do
        cap <- parseCapText
        case cap of
            "≡"                          -> return $ Just AssertEQ
            "static"                     -> return Nothing
            "asynch"                     -> return Nothing
            "compile"                    -> return Nothing
            "isolated"                   -> return Nothing
            "pure"                       -> return Nothing
            "safe"                       -> return Nothing
            "lazy"                       -> return Nothing
            "debug print raw"            -> return $ Just DebugPrintRaw
            "debug print text"           -> return $ Just DebugPrintText
            _ | "todo:" `isPrefixOf` cap -> return Nothing
            _ | "TODO:" `isPrefixOf` cap -> return Nothing
            _                            -> fail $ "Unrecognised capability: " ++ show cap
    parseCapText = many $ noneOf "{}\n"

parseText :: Parser String
parseText = between (char '"') (string "\n~") $ intercalate "\n" <$> parseLines
  where
    parseLines = sepBy (many $ notChar '\n') $ string "\n "

parseBlock :: Parser [RawOp]
parseBlock = between (char '[') (char ']') parse'

parse' :: Parser [RawOp]
parse' = catMaybes <$> many ( oneOf " \n" *> return Nothing
                          <|> fmap Op <$> parseCap
                          <|> Just <$> ( LitBlock <$> parseBlock
                                     <|> Op <$> (LitText <$> parseText <|> try parseOp)
                                       )
                            )

parse :: String -> IO (Maybe [RawOp])
parse input =
    case parseString parse' (Lines 0 0 0 0) input of
        Success a  -> return (Just a)
        Failure xs -> do
            Pretty.displayIO stderr $ Pretty.renderPretty 0.8 80 $ xs <> Pretty.linebreak
            return Nothing
