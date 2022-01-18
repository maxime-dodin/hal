import Test.HUnit
import TestTools
import Arithmetics

testBasics :: Test
testBasics = TestList [
    TestLabel "testOneNumber"   $ testLispResult "1" "1",
    TestLabel "testOneBool"     $ testLispResult "#t" "#t",
    -- TestLabel "testOneNil"     $ testLispResult "()" "error",
    TestLabel "testOneBool"     $ testLispResult "#t" "#t",
    TestLabel "testOneElemList" $ testLispResult "(1)" (errorProc "1"),
    TestLabel "testFuncConsDot" $ testLispResult "(+ . (1 . (2 . ())))" "3",

    TestLabel "testQuoteLetter" $ testLispResult "'a" "a",
    TestLabel "testQuoteProcedure" $ testLispResult "'+" "+",
    TestLabel "testQuoteCons"   $ testLispResult "'(1 . 2)" "(1 . 2)",
    TestLabel "testQuoteList"   $ testLispResult "'(1 2 3)" "(1 2 3)",
    -- TestLabel "testQuoteConsList" $ testLispResult "'(1 2 . 3)" "(1 2 . 3)",
    TestLabel "testQuoteInPlus" $ testLispResult "(+ '1 2)" "3",

    TestLabel "testConsNums"            $ testLispResult "(cons 1 2)" "(1 . 2)",
    TestLabel "testConsProcedures"      $ testLispResult "(cons + -)" "(#<procedure +> . #<procedure ->)",
    TestLabel "testConsErrorComplex"    $ testLispResult "(cons 1 '())" "(1)",
    TestLabel "testConsQuote"           $ testLispResult "(cons 1 '())" "(1)",

    TestLabel "testAtomNum"     $ testLispResult "(atom? 1)"    "#t",
    TestLabel "testAtomBool"    $ testLispResult "(atom? #t)"   "#t",
    TestLabel "testAtomString"  $ testLispResult "(atom? 'foo)" "#t",
    TestLabel "testAtomNil"     $ testLispResult "(atom? '())"  "#t",
    TestLabel "testAtomList"    $ testLispResult "(atom? '(1 2 3))"  "#f",
    TestLabel "testAtomNil"     $ testLispResult "(atom? 1 2)"  "atom?: wrong number of arguments, only one expected.",

    TestLabel "testEqSimpleNums"    $ testLispResult "(eq? 1 1)"  "#t",
    TestLabel "testEqSameBool"      $ testLispResult "(eq? #t #t)"  "#t",
    TestLabel "testEqTrueFalse"     $ testLispResult "(eq? #t #f)"  "#f",
    TestLabel "testEqSimpleStrings" $ testLispResult "(eq? 'foo 'foo)"  "#t",
    TestLabel "testEqSimpleLists"   $ testLispResult "(eq? '(1 2) '(1 2))"  "#f",
    TestLabel "testEqStringCar"     $ testLispResult "(eq? 'foo (car '(foo bar)))"  "#t",
    TestLabel "testEqAdd"           $ testLispResult "(eq? (+ 1 1) 2)"  "#t",

    TestLabel "testDefineNum"               $ testLispResult "(define foo 1)foo"  "1",
    TestLabel "testDefineErrorNums"         $ testLispResult "(define 1 1)"  "define: invalid syntax",
    TestLabel "testDefineFunc"              $ testLispResult "(define add +) (add 1 2)"  "3",
    TestLabel "testDefineLambda"            $ testLispResult "(define add (lambda (a b) (+ a b))) (add 1 2)"  "3",
    TestLabel "testDefineImplicitLambda"    $ testLispResult "(define (add a b) (+ a b)) (add 1 2)"  "3",

    TestLabel "testLambdaBasic"      $ testLispResult "((lambda (a b) (+ a b)) 1 2)"  "3",
    TestLabel "testLambdaAdvance"    $ testLispResult "((lambda (a b c d) (+ (* b c) (div b a))) 1 2 3 4)"  "8",
    TestLabel "testLambdaProcedure"  $ testLispResult "(lambda (a b) (+ a b))"  "#<procedure>",
    TestLabel "testLambdaErr"        $ testLispResult "((lambda (a b c) (+ a b)) 1 2)"  "env: invalid syntax"
    ]

main :: IO Counts
main = runTestTT testBasics >> runTestTT testArithmetics