-- Modify the previous exercise to support \n, \r, \t, \\, and any other
-- desired escape characters.

module Ex3 where

import Text.ParserCombinators.Parsec
import WYAS48H

{-# ANN module "HLint: ignore Use string literal" #-}

escapedChars :: Parser Char
escapedChars = do
               char '\\'
               c <- oneOf ['"', '\\', 'n', 'r', 't']
               return $ case c of
                            'n' -> '\n'
                            'r' -> '\r'
                            't' -> '\t'
                            _   -> c

parseString :: Parser LispVal
parseString = do
              char '"'
              s <- many $ escapedChars <|> noneOf "\""
              char '"'
              return (String s)
