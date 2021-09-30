{-# LANGUAGE LambdaCase, TupleSections #-}

module Parser where

import Control.Applicative (Alternative(..))
import Data.Char (isDigit, digitToInt)

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ fmap (fmap f) . p

instance Applicative Parser where
    pure x = Parser $ pure . (, x)
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do 
        (input', f) <- p1 input
        fmap f <$> p2 input'

instance Alternative Parser where
    empty = Parser $ const empty
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
    (Parser p) >>= f = Parser $ \input -> do
        (input', x) <- p input
        runParser (f x) input'

check :: (Char -> Bool) -> Parser Char
check predicate = Parser $ \case
    (x:xs) | predicate x -> Just (xs, x)
    _                    -> Nothing

spanCheck :: (Char -> Bool) -> Parser String
spanCheck predicate = many (check predicate)

notEmpty :: Parser [a] -> Parser [a]
notEmpty (Parser p) = Parser $ \input -> do
    (input', x:xs) <- p input
    Just (input', x:xs)

surroundedBy :: Parser a -> Parser b -> Parser a
surroundedBy inner out = out *> inner <* out

separatedBy :: Parser a -> Parser b -> Parser [a]
separatedBy p sep = ((:) <$> p <*> many (sep *> p)) <|> pure [] 

char :: Char -> Parser Char
char c = check (== c)

spaces :: Parser String
spaces = many (char ' ' <|> char '\t' <|> char '\n')

digit :: Parser Int 
digit = digitToInt <$> check isDigit

string :: String -> Parser String
string "" = pure ""
string (c:cs) = (:) <$> char c <*> string cs

literalString :: Parser String
literalString = spanCheck (/= '"') `surroundedBy` char '"'

list parser lbracket rbracker sep = lbracket *> elements <* rbracker
    where
        elements = parser `surroundedBy` spaces `separatedBy` sep