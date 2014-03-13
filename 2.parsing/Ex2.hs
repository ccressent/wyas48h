-- Change parseString so that \" gives a literal quote character instead of
-- terminating the string. You may want to replace noneOf "\"" with a new
-- parser action that accepts either a non-quote character or a backslash
-- followed by a quote mark.

module Ex2 where

import Text.ParserCombinators.Parsec
import WYAS48H

parseString :: Parser LispVal
parseString = do 
              char '"'
              s <- many $ (char '\\' >> char '"') <|> noneOf "\""
              char '"'
              return (String s)
