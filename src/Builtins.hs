module Builtins where

import Sexpr
import MathsBuiltins

getMapFunc :: [(String, Sexpr -> Either String Sexpr)]
getMapFunc = [
    ("cons", cons), ("car", car), ("cdr", cdr),
    ("atom?", atom), ("eq?", eq),
    ("+", plus), ("-", minus), ("*", multiple), ("div", division), ("mod", modulo), ("<", lower),
    ("quote", quote)
    ]

quote :: Sexpr -> Either String Sexpr
quote (Cons x Nil) = Right x
quote _ = Left "quote: wrong number of arguments, only one expected."

cons :: Sexpr -> Either String Sexpr
cons (Cons x (Cons y Nil)) = Right (Cons x y)
cons _ = Left "cons: wrong number of arguments, only two expected."

car :: Sexpr -> Either String Sexpr
car (Cons (Cons x Nil) y) = Left $ "car: " ++ show x ++ " is not a pair."
car (Cons (Cons x x') y) = Right x
car _ = Left "car: wrong number of arguments, only two expected."

cdr :: Sexpr -> Either String Sexpr
cdr (Cons (Cons x Nil) y) = Left $ "cdr: " ++ show x ++ " is not a pair."
cdr (Cons (Cons x x') y) = Right x'
cdr _ = Left "cdr: wrong number of arguments, only two expected."

atom :: Sexpr -> Either String Sexpr
atom (Cons (Cons _ _) Nil) = Right $ Bool False
atom (Cons _ Nil) = Right $ Bool True
atom _ = Left "atom?: wrong number of arguments, only one expected."

eq :: Sexpr -> Either String Sexpr
eq (Cons (Cons x x') (Cons y Nil)) = Right $ Bool False
eq (Cons x (Cons (Cons y y') Nil)) = Right $ Bool False
eq (Cons x (Cons y Nil))
    | x == y = Right $ Bool True
    | otherwise = Right $ Bool False
eq _ = Left "eq?: wrong number of arguments, only two expected."