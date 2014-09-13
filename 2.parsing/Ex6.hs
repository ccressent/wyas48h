-- Add a Float constructor to LispVal, and support R5RS syntax for decimals.

module Ex6 where

import Text.ParserCombinators.Parsec

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | String String
             | Bool Bool
             deriving(Show)


parseFloat :: Parser LispVal
parseFloat = do
             beforeDot <- many1 digit
             char '.'
             afterDot <- many1 digit
             return $ (Float . read) (beforeDot ++ "." ++ afterDot)
