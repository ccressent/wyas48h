-- Rewrite parseNumber, without liftM, using
--   1. do-notation
--   2. explicit sequencing with the >>= operator

module Ex1 where

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec
import Wyas48h hiding (parseNumber)

-- Using liftM
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- Using do-notation
parseNumber' :: Parser LispVal
parseNumber' = do
               number <- many1 digit
               return $ (Number . read) number

-- Using >>=
parseNumber'' :: Parser LispVal
parseNumber'' = many1 digit >>= \x -> return $ (Number . read) x
