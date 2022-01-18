module TestTools where

import Test.HUnit
import Data.Either
import Parser
import Hal
import Sexpr

errorProc :: String -> String
errorProc x = "attempt to apply non-procedure " ++ x

formatError :: String -> String
formatError lisp = "For the following LISP program:\n" ++ lisp

testLispResult :: String -> String -> Test
testLispResult content result = TestCase (assertEqual (formatError content) result getResult)
    where
        getResult = case runHal [] content of
            Right (env, result) -> show result
            Left error -> error