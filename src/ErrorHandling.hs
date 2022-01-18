module ErrorHandling where

import GHC.IO.Exception (ExitCode(ExitFailure))
import System.Exit (exitWith)

usage :: String
usage = "USAGE: ./hal [file] [-i]\n\n\
\ no args: interactive mode (REPL) only\n\
\ file: scheme files you'd want to interpret \n\
\ -i : combining files and interactive mode (REPL)\n"

putError :: String -> IO()
putError str = putStrLn ("error: " ++ str) >> exitWith (ExitFailure 84)

putErrorWithUsage :: String -> IO()
putErrorWithUsage str = putStr ("error: " ++ str ++ "\n\n") >> putStrLn usage >> exitWith (ExitFailure 84)

putUsage :: IO()
putUsage = putStrLn usage