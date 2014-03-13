-- Add a Character constructor to LispVal, and create a parser for character
-- literals as described in R5RS.

module Ex5 where

import Text.ParserCombinators.Parsec
import WYAS48H

characterName :: Parser String
characterName = string "newline" <|> string "space"

parseCharacter :: Parser LispVal
parseCharacter = do
                 char '#'
                 char '\\'
                 c <- anyChar
                 return $ Character c
