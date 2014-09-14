module Main where

import Control.Monad
import Control.Monad.Error
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

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
    show = showError

instance Error LispError where
    noMsg  = Default "An error has occurred"
    strMsg = Default

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
                                  ++ " args; found values " ++ unwordList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

type ThrowsError = Either LispError

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = action `catchError` (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

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

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse (spaces >> parseExpr) "lisp" input of
                      Left  err -> throwError $ Parser err
                      Right val -> return val

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

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _)   = return val

eval (List [Atom "if", predicate, conseq, alt]) =
    do result <- eval predicate
       case result of
         Bool False -> eval alt
         otherwise  -> eval conseq

eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+))
             ,("-", numericBinop (-))
             ,("*", numericBinop (*))
             ,("/", numericBinop div)
             ,("mod", numericBinop mod)
             ,("quotient", numericBinop quot)
             ,("remainder", numericBinop rem)
             ,("=",  numBoolBinop (==))
             ,("/=", numBoolBinop (/=))
             ,("<",  numBoolBinop (<))
             ,("<=", numBoolBinop (<=))
             ,(">",  numBoolBinop (>))
             ,(">=", numBoolBinop (>=))
             ,("&&", boolBoolBinop (&&))
             ,("||", boolBoolBinop (||))
             ,("string=?",  strBoolBinop (==))
             ,("string<?",  strBoolBinop (<))
             ,("string<=?", strBoolBinop (<=))
             ,("string>?",  strBoolBinop (>))
             ,("string>=?", strBoolBinop (>=))]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ []      = throwError $ NumArgs 2 []
numericBinop _ val@[_] = throwError $ NumArgs 2 val
numericBinop op params = liftM (Number . foldl1 op) $ mapM unpackNum params

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left  <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                         if null parsed
                           then throwError $ TypeMismatch "number" $ String n
                           else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number n) = return $ show n
unpackStr (Bool b)   = return $ show b
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

main :: IO ()
main = do
       args   <- getArgs
       evaled <- return $ liftM show $ readExpr (head args) >>= eval
       putStrLn $ extractValue $ trapError evaled
