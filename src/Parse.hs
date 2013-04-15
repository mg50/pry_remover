module Parse (PryInstance(..), getPryInstances) where

import Text.ParserCombinators.Parsec
import Data.List
import Control.Monad

data PryInstance = PryInstance { fileName :: String, lineNums :: [Int]  } deriving (Eq, Show)

pryLineParser :: Parser (String, Int)
pryLineParser = do fileName <- many1 $ alphaNum <|> oneOf "/."
                   char ':'
                   many space
                   lineNumber <- liftM read $ many1 digit
                   char ':'
                   many space
                   many1 anyChar
                   many space
                   return (fileName, lineNumber)

getPryInstances :: String -> [PryInstance]
getPryInstances input = case parse (many pryLineParser) "" input of
  Left err     -> error $ "could not parse input: " ++ show err
  Right places -> let fileNames = nub $ map fst places
                      linesOf name = sort [num | (x, num) <- places, x == name]
                  in [PryInstance file (linesOf file) | file <- fileNames]
