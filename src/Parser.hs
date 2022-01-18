module Parser where

import Control.Applicative (Alternative((<|>)))
import Data.Foldable (find)

import ParsingTools
import Sexpr
import Builtins (getMapFunc)

parseQuote :: Parser Sexpr
parseQuote = toQuote <$> (parseChar '\'' *> parseSexpr)
    where toQuote result = Cons (Proc "quote") (Cons result Nil)

parseNil :: Parser Sexpr
parseNil = Nil <$ (parseChar '(' *> parseChar ')')

parseString :: Parser Sexpr
parseString = Sym <$> parseSome (parseAnyChar (['!'..'\''] ++ ['*' ..  '-'] ++ "/" ++ [':' .. '~']))

parseAtom :: Parser Sexpr
parseAtom = (Num <$> parseInt)
    <|> (Bool <$> parseLispBool)
    <|> parseNil
    <|> parseString

parseCons :: Parser Sexpr
parseCons = Cons <$> (parseChar '(' *> parseSexpr) <*> (parseChar '.' *> parseSexpr <* parseChar ')')

parseMembers :: Parser Sexpr
parseMembers = Cons <$> parseSexpr <*> (parseMembers <|> pure Nil)

parseList :: Parser Sexpr
parseList = parseChar '(' *> parseMembers <* parseChar ')'

parseSexpr :: Parser Sexpr
parseSexpr = parseSpace $ parseQuote <|> parseCons <|> parseList <|> parseAtom