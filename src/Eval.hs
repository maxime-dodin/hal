module Eval where

import Data.Foldable (find)
import Data.Bits (Bits(xor))

import ParsingTools ( Parser(runParser) )
import Builtins ( getMapFunc, quote )
import Sexpr

apply :: Sexpr -> Sexpr -> Either String Sexpr
apply (Proc x) y = case find ((==x).fst) getMapFunc of
    Nothing -> Left $ "attempt to apply non-procedure " ++ x
    Just (key, value) -> value y
apply (Cons (Sym "lambda") x) y = lambda (Cons (Cons (Sym "lambda") x) y)
apply x _ = Left $ "attempt to apply non-procedure " ++ show x

createEnv :: Sexpr -> Sexpr -> [(String, Sexpr)] -> Either String [(String, Sexpr)]
createEnv (Cons (Sym var) Nil) (Cons val Nil) env = Right $ (var, val):env
createEnv (Cons (Sym var) x) (Cons val y) env = case createEnv x y ((var, val):env) of
    Right x -> Right x
    Left err -> Left err
createEnv _ _ env = Left "env: invalid syntax"

lambda :: Sexpr -> Either String Sexpr
lambda (Cons (Cons _ (Cons var (Cons fct Nil))) val) = case createEnv var val [] of
    Right x -> eval x fct
    Left err -> Left err
lambda _ = Left "lambda: invalid syntax"

define :: [(String, Sexpr)] -> Sexpr -> Either String ([(String, Sexpr)], Sexpr)
define env (Cons (Sym key) (Cons sexpr@(Cons (Sym "lambda") args) Nil)) = Right ((key,sexpr):env, Proc key)
define env (Cons (Cons (Sym key) vars) (Cons func@(Cons x x') Nil)) = Right ((key, Cons (Sym "lambda") (Cons vars (Cons func Nil))):env, Proc key)
define env (Cons (Sym key) (Cons value Nil)) = case eval env value of
    Left error -> Left error
    Right value' -> Right ((key,value'):env, Proc key)
define _ _ = Left "define: invalid syntax"

eval :: [(String, Sexpr)] -> Sexpr -> Either String Sexpr
eval env (Cons (Proc "quote") y) = quote y
eval env expr@(Cons (Cons (Sym "lambda") (Cons x y)) y') = lambda expr
eval env expr@(Cons (Sym "lambda") (Cons x y)) = Right $ Proc ""
eval env (Cons x y) = case evalCons env x of
    Left error -> Left error
    Right x' -> case evalCons env y of
        Left error -> Left error
        Right y' -> apply x' y'
eval env (Sym x) = case find ((==x).fst) getMapFunc of
    Just (key, value) -> Right $ Proc key
    Nothing -> case find ((==x).fst) env of
        Nothing -> Left $ "variable " ++ x ++ " is not bound"
        Just (key, value) -> Right value
eval env x = Right x

evalCons :: [(String, Sexpr)] -> Sexpr -> Either String Sexpr
evalCons env (Cons x y) = case eval env x of
    Left error -> Left error
    Right x' -> case evalCons env y of
        Left error -> Left error
        Right y' -> Right (Cons x' y')
evalCons env x = eval env x
