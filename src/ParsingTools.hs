module ParsingTools where

{-# LANGUAGE TupleSections #-}

import Data.Either
import Text.Read (readEither)
import Control.Applicative ( Alternative((<|>), empty) )
import GHC.Unicode (isSpace)

newtype Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

instance Functor Parser where
    fmap fct parser = Parser fM where
        fM str = case runParser parser str of
            Left x -> Left x
            Right (v,w) -> Right (fct v, w)
instance Applicative Parser where
    pure a = Parser f where
        f str = Right (a, str)
    (<*>) f x = Parser fStar where
        fStar str = case runParser f str of
            Left msg -> Left msg
            Right (v,v') -> case runParser x v' of
                Left msg -> Left msg
                Right (w,w') -> Right (v w, w')

instance Alternative Parser where
    (<|>) a b = Parser func where
        func str = case runParser a str of
            Left x -> runParser b str
            Right x -> Right x
    empty = Parser (const (Left "empty"))

instance Monad Parser where
    return = pure
    (>>=) a f = Parser p where
        p str = case runParser (f <$> a) str of
            Right (b, str) -> runParser b str
            Left str -> Left str

joinArgs :: Monad m => m (m a) -> m a
joinArgs m = m >>= id

parseChar :: Char -> Parser Char
parseChar char = Parser func where
    func [] = Left "error: empty string to scan."
    func (x:xs)
        | char == x = Right (x,xs)
        | otherwise = Left ("can't find \'" ++ [char] ++ "\' in \'" ++ (x:xs) ++ "\'")

-- to do with elem flip
parseAnyChar :: String -> Parser Char
parseAnyChar [] = Parser func where
    func [] = Left "error: parseAnyChar: empty string to scan"
    func x = Left "error: parseAnyChar: char not found"
parseAnyChar (x:xs) = Parser func where
    func str = case runParser (parseChar x) str of
        Left _ -> runParser (parseAnyChar xs) str
        Right (x,xs) -> Right (x,xs)

parseOr :: Parser a -> Parser a -> Parser a
parseOr a b = Parser func where
    func str = case runParser a str of
        Left x -> runParser b str
        Right x -> Right x

parseAnd :: Parser a -> Parser b -> Parser (a,b)
parseAnd a b = Parser func where
    func str = case runParser a str of
        Right (v,w) -> case runParser b w of
            Left msg -> Left msg
            Right (x,y) -> Right ((v,x), y)
        Left msg -> Left msg

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith func a b = Parser pAW where
    pAW str = case runParser (parseAnd a b) str of
        Left x -> Left x
        Right ((v,w), y) -> Right (func v w, y)

parseMany ::  Parser a -> Parser [a]
parseMany func = Parser pMany where
    pMany str = case runParser func str of
        Right (x, y) -> case runParser (parseMany func) y of
            Right (v,w) -> Right (x:v, w)
            Left x -> Right ([], str)
        Left x -> Right ([], str)

parseSome :: Parser a -> Parser [a]
parseSome func = Parser pSome where
    pSome str = case runParser func str of
        Right (x, y) -> case runParser (parseMany func) y of
            Right (v,w) -> Right (x:v, w)
            Left x -> Right ([], str)
        Left x -> Left x

-- to do with if then else / if head s jsp quoi
parseCheck :: (Char -> Bool) -> Parser Char
parseCheck f = Parser $ \s -> case s of
  (x:xs) | f x -> Right (x, xs)
  _            -> Left "error: invalid char"

parseSpace :: Parser a -> Parser a
parseSpace p = blanks *> p <* blanks
    where
        blanks = parseMany (parseCheck isSpace)

parseSign :: Parser Int
parseSign = parseOr ((-1) <$ parseChar '-') (pure 1)

readInt :: String -> Either String Int
readInt = readEither

parseUInt :: Parser Int
parseUInt = Parser f where
    f str = case runParser (readInt <$> parseSome (parseAnyChar "0123456789.")) str of
        Left msg -> Left msg
        Right (Right nb, s) -> Right (nb,s)
        Right (Left x, s) -> Left "error: parser"

parseInt :: Parser Int
parseInt = (*) <$> parseSign <*> parseUInt

readLispBool :: String -> Either String Bool
readLispBool ('#':'t':_) = Right True
readLispBool ('#':'f':_) = Right False
readLispBool _ = Left "bool not found"

parseLispBool :: Parser Bool
parseLispBool = Parser p where
    p str = case runParser (readLispBool <$> parseSome (parseAnyChar "#tf")) str of
        Left msg -> Left msg
        Right (Right bool, s) -> Right (bool,s)
        Right (Left x, s) -> Left "error: bool not found"
