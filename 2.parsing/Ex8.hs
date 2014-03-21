-- Add support for the backquote syntactic sugar.
-- The following doesn't make sure that unquoted and unquoted-splicing
-- are within a quasiquoted expression...

module Ex8 where

import Text.ParserCombinators.Parsec
import Wyas48h

parseBackquoted :: Parser LispVal
parseBackquoted = parseQuasiquoted <|> try parseUnquotedSplicing <|> parseUnquoted

parseQuasiquoted :: Parser LispVal
parseQuasiquoted = do
                  char '`'
                  expression <- parseExpr
                  return $ List [Atom "quasiquote", expression]

parseUnquoted :: Parser LispVal
parseUnquoted = do
                char ','
                expression <- parseExpr
                return $ List [Atom "unquote", expression]

parseUnquotedSplicing :: Parser LispVal
parseUnquotedSplicing = do
                        string ",@"
                        expression <- parseExpr
                        return $ List [Atom "unquote-splicing", expression]
