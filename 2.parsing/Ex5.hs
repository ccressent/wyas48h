-- Add a Character constructor to LispVal, and create a parser for character
-- literals as described in R5RS.

module Ex5 where

import Data.Char (isPrint)
import Text.ParserCombinators.Parsec

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Character Char
             | String String
             | Bool Bool
             deriving(Show)

printable :: Parser Char
printable = satisfy isPrint

characterName :: Parser Char
characterName = do
                name <- string "newline" <|> string "space"
                return $ case name of
                              "newline" ->  '\n'
                              "space"   ->  ' '

parseCharacter :: Parser LispVal
parseCharacter = do
                 string "#\\"
                 c <- try characterName <|> printable
                 return $ Character c
