-- Modify the previous exercise to support \n, \r, \t, \\, and any other
-- desired escape characters.
-- R5RS says "Scheme does not specify the effect of a backslash within a string
-- that is not followed by a doublequote or backslash". We support \n, \r and
-- \t nonetheless for the sake of solving the exercice.

module Ex3 where

import Text.ParserCombinators.Parsec
import Wyas48h hiding (parseString)

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
