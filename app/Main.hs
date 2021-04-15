module Main where

import System.Environment (getArgs) 
import System.Exit (exitFailure) 
import Control.Monad (when)
import Automata.Parser
import Automata.Automata
  
main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) $ putStrLn "INVALID ARGUMENTS" >> exitFailure

  let [path, expr] = args
  automata <- parseAutomata <$> readFile path
  if not $ isDeterministic automata then putStrLn "ERROR"
  else putStrLn $ if accepts automata expr then "YES" else "NO"