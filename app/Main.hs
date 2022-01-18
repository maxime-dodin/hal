module Main where

import System.Environment (getArgs)
import GHC.Exts (IsList(fromList))
import System.IO ( stdout, hFlush )

import Hal ( runHal )
import Sexpr ( Sexpr )
import Eval ( eval )
import Parser (parseSexpr)
import ParsingTools (Parser(runParser))
import ErrorHandling (putError, putErrorWithUsage, putUsage)
-- import System.Directory (doesFileExist)

main :: IO ()
main = getArgs >>= launch

launch  :: [String] -> IO ()
launch args
  | null args = launchExec args [] True
  | "-i" `elem` args = launchExec (filter (/= "-i") args) [] True
  | otherwise = launchExec args [] False

launchExec :: [String]  -> [(String, Sexpr)] -> Bool -> IO()
launchExec files env isInter = do
    mapM readFile files >>= handleFile env isInter

--    _ <- filesExist files >> return ()
-- filesExist :: [String] -> IO ()
-- filesExist (x:xs) = do
--   exist <- doesFileExist x
--   handle exist
--   where
--     handle exist
--       | exist && null xs = return ()
--       | exist = filesExist xs >> return ()
--       | otherwise = putError "invalid file"
-- filesExist _ = return ()


loopInterpreter :: [(String, Sexpr)] -> IO()
loopInterpreter env = putStr "> " >> hFlush stdout >> getLine >>= start
    where
        start str = case runHal env str of
            Right (env, sexpr) -> print sexpr >> loopInterpreter env
            Left err -> putError err >> loopInterpreter env

-- Left err -> print $ "error: " ++ err //continue repl when error

handleFile :: [(String, Sexpr)] -> Bool -> [String] -> IO()
handleFile env False [] = putError "no more file"
handleFile env True [] = loopInterpreter env
handleFile env isInter files@(hd:tl)
    | null files && not isInter = putError "no more file"
    | length files == 1 && not isInter = case hal of
        Right (env', result) -> print result
        Left x -> putError x
    | otherwise = case hal of
        Right (env', res) -> handleFile env' isInter tl
        Left x -> putError x
    where
        hal = runHal env (head files)

-- | Check if all files exists
-- checkFiles :: [String] -> [(String, Sexpr)]
-- checkFiles (x:xs) = do
--   exist <- doesFileExist x
--   handle exist
--   where
--     handle exist
--       | exist && length xs == 0 = return []
--       | exist = checkFiles xs >> return []
--       | otherwise = (exitWithErr $ "*** ERROR : " ++ x ++ " no such file") >> return []