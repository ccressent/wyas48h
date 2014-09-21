{-# LANGUAGE ExistentialQuantification #-}

module Chapter9 where

import Control.Monad
import Control.Monad.Error
import Data.IORef
import Text.ParserCombinators.Parsec
import System.Environment
import System.IO

{-# ANN module "HLint: ignore Use string literal" #-}

type Env = IORef [(String, IORef LispVal)]
type IOThrowsError = ErrorT LispError IO

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
     then setVar envRef var value >> return value
     else liftIO $ do
          valueRef <- newIORef value
          env      <- readIORef envRef
          writeIORef envRef ((var, valueRef) : env)
          return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Port Handle
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Func { params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env }

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
showVal (Port _)          = "<IO port>"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (IOFunc _)        = "<IO primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
    "(lamda (" ++ unwords (map show args) ++
        (case varargs of
           Nothing  -> ""
           Just arg -> " . " ++ arg) ++ ") ...)"

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

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
    do unpacked1 <- unpacker arg1
       unpacked2 <- unpacker arg2
       return $ unpacked1 == unpacked2
    `catchError` (const $ return False)

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

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
                             Left err  -> throwError $ Parser err
                             Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

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

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _)   = return val
eval env (Atom id)      = getVar env id

eval env (List [Atom "if", predicate, conseq, alt]) =
    do result <- eval env predicate
       case result of
         Bool False -> eval env alt
         otherwise  -> eval env conseq

eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "load", String filename]) =
    load filename >>= liftM last . mapM (eval env)
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarArgs varargs env [] body
eval env (List (function : args)) = do
  func    <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals

eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args        = func args
apply (Func params varargs body closure) args =
    if num params /= num args && varargs == Nothing
    then throwError $ NumArgs (num params) args
    else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body
          bindVarArgs arg env = case arg of
                                  Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                                  Nothing      -> return env

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc)
               ,("open-input-file", makePort ReadMode)
               ,("open-output-file", makePort WriteMode)
               ,("close-input-file", closePort)
               ,("close-output-file", closePort)
               ,("read", readProc)
               ,("write", writeProc)
               ,("read-contents", readContents)
               ,("real-all", readAll)]

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
             ,("string>=?", strBoolBinop (>=))
             ,("car",  car)
             ,("cdr",  cdr)
             ,("cons", cons)
             ,("eq?",    eqv)
             ,("eqv?",   eqv)
             ,("equal?", equal)]

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                              ++ map (makeFunc PrimitiveFunc) primitives)
    where makeFunc constructor (var, func) = (var, constructor func)

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

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

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)]         = return x
car [DottedList (x:_) _] = return x
car [badArg]             = throwError $ TypeMismatch "pair" badArg
car badArgList           = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg]              = throwError $ TypeMismatch "pair" badArg
cdr badArgList            = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs]  = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2]                = return $ DottedList [x1] x2
cons badArgList              = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1),   (Bool arg2)]           = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1),   (Atom arg2)]           = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2)
                                                          && (all eqvPair $ zip arg1 arg2)
    where eqvPair (x, y) = case eqv [x, y] of
                             Left _           -> False
                             Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool $ False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal [x, y] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals x y)
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [x, y]
  return $ Bool $ (primitiveEquals || let (Bool b) = eqvEquals in b)
equal badArgList = throwError $ NumArgs 2 badArgList


flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

readPrompt :: String -> IO String
readPrompt p = flushStr p >> getLine

evalString :: Env -> String -> IO String
evalString env s = runIOThrows $ liftM show $ (liftThrows $ readExpr s) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env s = evalString env s >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  result <- prompt
  if predicate result
     then return ()
     else action result >> until_ predicate prompt action

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
      >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt ">>") . evalAndPrint

main :: IO ()
main = do
       args   <- getArgs
       if null args then runRepl else runOne $ args
