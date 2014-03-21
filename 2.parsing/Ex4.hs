-- Change parseNumber to support the Scheme standard for different bases.

module Ex4 where

import Numeric (readHex, readOct)
import Text.ParserCombinators.Parsec
import Wyas48h hiding (parseNumber)

{-# ANN module "HLint: ignore Use string literal" #-}

data Radix = Binary | Octal | Decimal | Hexadecimal
           deriving(Show)

radixPrefix :: Parser Radix
radixPrefix = do
              char '#'
              radix <- oneOf ['b', 'o', 'd', 'x']
              return $ case radix of
                            'b' -> Binary
                            'o' -> Octal
                            'd' -> Decimal
                            'x' -> Hexadecimal

parseNumber :: Parser LispVal
parseNumber = do
               radix <- optionMaybe radixPrefix
               case radix of
                    Just Binary      -> parseBinaryNumber
                    Just Octal       -> parseOctalNumber
                    Just Decimal     -> parseDecimalNumber
                    Just Hexadecimal -> parseHexadecimalNumber
                    Nothing          -> parseDecimalNumber

isBinDigit :: Char -> Bool
isBinDigit c = c == '0' || c == '1'

binDigit :: Parser Char
binDigit = satisfy isBinDigit

readBin :: Integral a => String -> a
readBin = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1

parseBinaryNumber :: Parser LispVal
parseBinaryNumber = do
                    n <- many1 binDigit
                    return $ (Number . readBin) n

parseOctalNumber :: Parser LispVal
parseOctalNumber = do
                   n <- many1 octDigit
                   return $ (Number . fst . head . readOct) n

parseDecimalNumber :: Parser LispVal
parseDecimalNumber = do
                     n <- many1 digit
                     return $ (Number . read) n

parseHexadecimalNumber :: Parser LispVal
parseHexadecimalNumber = do
                         n <- many1 hexDigit
                         return $ (Number . fst . head . readHex) n
