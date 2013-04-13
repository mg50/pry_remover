module Parse (PryInstance(..), getPryInstances) where

import Text.ParserCombinators.Parsec
import Data.List
import Control.Monad

data PryInstance = PryInstance { fileName :: String, lineNums :: [Int]  } deriving (Eq, Show)

pryLineParser :: Parser (String, Int)
pryLineParser = do fileName <- many1 $ alphaNum <|> char '/'
                   newline
                   lineNumber <- liftM read $ many1 digit
                   many newline
                   return $ (fileName, lineNumber)

getPryInstances :: String -> [PryInstance]
getPryInstances input = case parse (many pryLineParser) "" input of
  Left err     -> error $ "could not parse input: " ++ show err
  Right places -> let fileNames = nub $ map fst places
                      linesOf name = [num | (x, num) <- places, x == name]
                  in [PryInstance file (linesOf file) | file <- fileNames]
