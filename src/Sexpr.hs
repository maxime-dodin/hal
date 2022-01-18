module Sexpr where

data Sexpr =
    Cons Sexpr Sexpr
    | Sym String
    | Bool Bool
    | Num Int
    | Proc String
    | Nil
    deriving (Eq)

showListMembers :: Sexpr -> [Char]
showListMembers (Cons x (Cons y z)) = show x ++ " " ++ showListMembers (Cons y z)
showListMembers (Cons x Nil) = show x
showListMembers (Cons x y) = show x ++ " . " ++ show y
showListMembers x = show x

instance Show Sexpr
    where
        show (Cons x (Cons y z)) = "(" ++ showListMembers (Cons x (Cons y z)) ++ ")"
        show (Cons x Nil) = "(" ++ show x ++ ")"
        show (Cons x y) = "(" ++ show x ++ " . " ++ show y ++ ")"
        show (Bool False) = "#f"
        show (Bool True) = "#t"
        show (Num x) = show x
        show (Proc "") = "#<procedure>"
        show (Proc x) = "#<procedure " ++ x ++ ">"
        show (Sym x) = x
        show Nil = "()"
