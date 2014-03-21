module Wyas48h where

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec
import System.Environment (getArgs)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving(Show)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do
            first <- letter <|> symbol
            rest  <- many (alphaNum <|> symbol)
            let atom = first : rest
            case atom of
                 "#t" -> return $ Bool True
                 "#f" -> return $ Bool False
                 _    -> return $ Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseString :: Parser LispVal
parseString = do char '"'
                 s <- many $ noneOf "\""
                 char '"'
                 return (String s)

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do
            char '('
            expression <- try parseList <|> parseDottedList
            char ')'
            return expression

readExpr :: String -> String
readExpr input = case parse (spaces >> parseExpr) "lisp" input of
                      Left  err -> "No match: " ++ show err
                      Right val -> "Found value: " ++ show val

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
                  listHead <- parseExpr `endBy` spaces
                  listTail <- char '.' >> spaces >> parseExpr
                  return $ DottedList listHead listTail

parseQuoted :: Parser LispVal
parseQuoted = do
              char '\''
              expression <- parseExpr
              return $ List [Atom "quote", expression]

main :: IO ()
main = do
       args <- getArgs
       putStrLn $ readExpr $ head args
