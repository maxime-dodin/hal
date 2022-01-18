module MathsBuiltins where

import Sexpr

plus :: Sexpr -> Either String Sexpr
plus Nil = Right $ Num 0
plus (Cons (Num x)  Nil) = Right $ Num x
plus (Cons (Num x) (Cons (Num y) Nil)) = Right $ Num $ x + y
plus (Cons (Num x) (Cons (Num y) y')) = plus (Cons (Num (x + y)) y')
plus x = Left $ "+: incorrect arguments: " ++ show x

minus :: Sexpr -> Either String Sexpr
minus (Cons (Num x)  Nil) = Right $ Num $ x * (-1)
minus (Cons (Num x) (Cons (Num y) Nil)) = Right $ Num $ x - y
minus (Cons (Num x) (Cons (Num y) y')) = minus (Cons (Num (x - y)) y')
minus Nil = Left "incorrect argument count in call (-)"
minus x = Left $ "-: incorrect arguments: " ++ show x

multiple :: Sexpr -> Either String Sexpr
multiple Nil = Right $ Num 1
multiple (Cons (Num x)  Nil) = Right $ Num x
multiple (Cons (Num x) (Cons (Num y) Nil)) = Right $ Num $ x * y
multiple (Cons (Num x) (Cons (Num y) y')) = multiple (Cons (Num (x * y)) y')
multiple x = Left $ "*: incorrect arguments: " ++ show x

division :: Sexpr -> Either String Sexpr
division (Cons (Num x) (Cons (Num y) Nil)) = Right $ Num $ x `div` y
division Nil = Left "incorrect argument count in call (div)"
division x = Left $"div: incorrect arguments: " ++ show x

modulo :: Sexpr -> Either String Sexpr
modulo (Cons (Num x) (Cons (Num y) Nil)) = Right $ Num $ x `mod` y
modulo Nil = Left "incorrect argument count in call (mod)"
modulo x = Left $ "mod: incorrect arguments: " ++ show x

lower :: Sexpr -> Either String Sexpr
lower (Cons (Num x)  Nil) = Right $ Bool True
lower (Cons (Num x) (Cons (Num y) Nil)) = if x < y then Right $ Bool True else Right $ Bool False
lower (Cons (Num x) (Cons (Num y) y')) = if x < y then lower (Cons (Num y) y') else lower (Cons (Num x) y')  --minus (Cons (Num (x - y)) y')
lower Nil = Left "incorrect argument count in call (<)"
lower x = Left $ "<: incorrect arguments: " ++ show x