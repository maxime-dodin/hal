module Hal where

import Parser ( parseSexpr )
import ParsingTools ( Parser(runParser) )
import Eval ( eval, define )
import Sexpr

-- fullEval :: [(String, Sexpr)] -> Sexpr -> Either String ([(String, Sexpr)], Sexpr)
-- fullEval env sexpr = case define env sexpr of
--         Right (env', res) -> Right (env', res)
--         Left error -> Left error

runHal :: [(String, Sexpr)] -> String ->  Either String ([(String, Sexpr)], Sexpr)
runHal env str = case runParser parseSexpr str of
    Left error -> Left error
    Right (Cons (Sym "define") y, "") -> define env y
    Right (Cons (Sym "define") y, to_parse) -> define env y >>= relaunch 
        where relaunch (env, result) = runHal env to_parse
    Right (tree, "") -> case eval env tree of
            Left error -> Left error
            Right sexpr -> Right (env, sexpr)
    Right (parsed, to_parse) -> runHal env to_parse