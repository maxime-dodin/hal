module Arithmetics where

import Test.HUnit
import TestTools

testArithmetics :: Test
testArithmetics = TestList [
    TestLabel "testAritmeticPlus basic"         $ testLispResult "(+ 1 2)"          "3",
    TestLabel "testAritmeticPlus advance"       $ testLispResult "(+ 1 2 3 4 5 6)"  "21",
    TestLabel "testAritmeticPlus symbol only"   $ testLispResult "(+)"              "0",
    TestLabel "testAritmeticPlus one number"    $ testLispResult " (+ 1)"            "1",

    TestLabel "testAritmeticMinus basic"        $ testLispResult "(- 1 2)"          "-1",
    TestLabel "testAritmeticMinus advance"      $ testLispResult "(- 1 2 3 4 5 6)"  "-19",
    TestLabel "testAritmeticMinus symbol only"  $ testLispResult "(-)"              "incorrect argument count in call (-)",
    TestLabel "testAritmeticMinus one number"   $ testLispResult "(- 1)"            "-1",

    TestLabel "testAritmeticMultiply basic"          $ testLispResult "(* 1 2)"          "2",
    TestLabel "testAritmeticMultiply advance"        $ testLispResult "(* 1 2 3 4 5 6)"  "720",
    TestLabel "testAritmeticMultiply symbol only"    $ testLispResult "(*)"              "1",
    TestLabel "testAritmeticMultiply one number"     $ testLispResult "(* 1)"            "1",

    TestLabel "testAritmeticDivision basic"          $ testLispResult "(div 1 2)"          "0",
    TestLabel "testAritmeticDivision advance"        $ testLispResult "(div 1 2 3 4 5 6)"  "div: incorrect arguments: (1 2 3 4 5 6)",
    TestLabel "testAritmeticDivision symbol only"    $ testLispResult "(div)"              "incorrect argument count in call (div)",
    TestLabel "testAritmeticDivision one number"     $ testLispResult "(div 1)"            "div: incorrect arguments: (1)",

    TestLabel "testAritmeticModulo basic"            $ testLispResult "(mod 1 2)"           "1",
    TestLabel "testAritmeticModulo advance"          $ testLispResult "(mod 1 2 3 4 5 6)"   "mod: incorrect arguments: (1 2 3 4 5 6)",
    TestLabel "testAritmeticModulo symbol only"      $ testLispResult "(mod)"               "incorrect argument count in call (mod)",
    TestLabel "testAritmeticModulo one number"       $ testLispResult "(mod 1)"             "mod: incorrect arguments: (1)",

    TestLabel "testAritmeticAllMixed basic"             $ testLispResult "(+ 1 (- 4 2) (* 2))"                      "5",
    TestLabel "testAritmeticAllMixed advence"           $ testLispResult "(+ 1 (- 4 (div 4 2)) (* (mod 2 4)))"      "5",
    TestLabel "testAritmeticAllMixed one error"         $ testLispResult "(+ 1 (- 4 (div)) (* 2))"                  "incorrect argument count in call (div)",
    -- TestLabel "testAritmeticAllMixed mutiple error"     $ testLispResult "(+ 1 (- 4 (mod)) (* (div)))"              "incorrect argument count in call (div)",

    TestLabel "testAritmeticMinus true"             $ testLispResult "(< 1 2)"           "#t",
    TestLabel "testAritmeticMinus false"            $ testLispResult "(< 2 1)"           "#f",
    TestLabel "testAritmeticMinus advance"          $ testLispResult "(< 1 2 3)"         "#t",
    TestLabel "testAritmeticMinus symbol only"      $ testLispResult "(<)"               "incorrect argument count in call (<)",
    TestLabel "testAritmeticMinus one number"       $ testLispResult "(< -1)"            "#t"
    ]