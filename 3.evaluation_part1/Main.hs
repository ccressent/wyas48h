module Main where

import Control.Monad 
import Text.ParserCombinators.Parsec
import System.Environment

{-# ANN module "HLint: ignore Use string literal" #-}

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where
    show = showVal

showVal :: LispVal -> String
showVal (Atom a)          = a
showVal (String s)        = "\"" ++ s ++ "\""
showVal (Number n)        = show n
showVal (Bool True)       = "#t"
showVal (Bool False)      = "#f"
showVal (List l)          = "(" ++ unwordList l ++ ")"
showVal (DottedList xs x) = "(" ++ unwordList xs ++ " . " ++ showVal x ++ ")"

unwordList :: [LispVal] -> String
unwordList = unwords . map showVal

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- R5RS says "Scheme does not specify the effect of a backslash within a string
-- that is not followed by a doublequote or backslash". We support \n, \r and
-- \t nonetheless for the sake of solving the exercice.
escapedChars :: Parser Char
escapedChars = do
               char '\\'
               c <- oneOf ['\\', '"', 'n', 'r', 't']
               return $ case c of
                            'n' -> '\n'
                            'r' -> '\r'
                            't' -> '\t'
                            _   -> c

parseAtom :: Parser LispVal
parseAtom = do 
            first <- letter <|> symbol 
            rest  <- many (alphaNum <|> symbol)
            let atom = first : rest
            case atom of
                 "#t" -> return $ Bool True
                 "#f" -> return $ Bool False
                 _    -> return $ Atom atom
-- Using liftM
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseString :: Parser LispVal
parseString = do char '"'
                 s <- many $ escapedChars <|> noneOf "\""
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
              
readExpr :: String -> LispVal
readExpr input = case parse (spaces >> parseExpr) "lisp" input of
                      Left  err -> String $ "No match: " ++ show err
                      Right val -> val

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

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _)   = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+))
             ,("-", numericBinop (-))
             ,("*", numericBinop (*))
             ,("/", numericBinop div)
             ,("mod", numericBinop mod)
             ,("quotient", numericBinop quot)
             ,("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                         if null parsed
                           then 0
                           else fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum _          = 0

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
