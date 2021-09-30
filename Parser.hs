{-# LANGUAGE LambdaCase, TupleSections #-}

module Parser where

import Control.Applicative (Alternative(..))
import Data.Char (isDigit, digitToInt)

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
    fmap f parser = Parser $ fmap (fmap f) . runParser parser

instance Applicative Parser where
    pure x = Parser $ pure . (, x)
    p1 <*> p2 = Parser $ \input -> do 
        (input', f) <- runParser p1 input
        fmap f <$> runParser p2 input'

instance Alternative Parser where
    empty = Parser $ const empty
    p1 <|> p2 = Parser $ \input -> runParser p1 input <|> runParser p2 input

check :: (Char -> Bool) -> Parser Char
check predicate = Parser $ \case
    (x:xs) | predicate x -> Just (xs, x)
    _                    -> Nothing

char :: Char -> Parser Char
char c = check (== c)

digit :: Parser Int 
digit = digitToInt <$> check isDigit

string :: String -> Parser String
string "" = pure ""
string (c:cs) = (:) <$> char c <*> string cs
