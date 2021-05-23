
-- | The main program.
module Main where

import SFPL.Base
import SFPL.Elab
import SFPL.Eval
import SFPL.Parser
import SFPL.Syntax.Core
import qualified SFPL.Syntax.Raw as R
import SFPL.Utils
import System.Environment
import System.Exit
import System.IO
import Text.Printf

displayHelpMessage :: IO ()
displayHelpMessage = do
  progName <- getProgName
  printf   "usage: %s [--help | <file>]\n" progName
  putStrLn "  --help:    display this help message"
  putStrLn "  <file>:    name of the source file"

parseSrc :: String -> String -> IO R.Program
parseSrc fileName src = do
  let res = parseProgram fileName src
  case res of
    Left bundle -> putStr (showPretty' bundle) >> exitSuccess
    Right prog  -> pure prog

elab :: SourceFile -> R.Program -> IO (EvalCxt, Lvl)
elab src prog = do
  let (res, metas) = elabProgram prog
  case res of
    Left errors -> putStr (showPretty (src, metas) errors) >> exitSuccess
    Right x     -> pure x

interpret :: String -> String -> IO ()
interpret fileName src = do
  rawProgram <- parseSrc fileName src
  let src' = arr $ lines src
  (cxt, mainLvl) <- elab src' rawProgram
  runMain cxt mainLvl

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"]  -> displayHelpMessage
    [fileName]  -> interpret fileName =<< readFile fileName
    _           -> displayHelpMessage

