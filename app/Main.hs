module Main where

import System.Environment (getArgs) 
import System.Exit (exitFailure) 
import Control.Monad (when)

import Grammar.CNFMaker (makeCNF)
import Grammar.Parser (parseGrammar)
import Grammar.CYK

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) $ putStrLn "INVALID ARGUMENTS" >> exitFailure

  let [path, expr] = args
  grammar <- makeCNF . parseGrammar <$> readFile path
  putStrLn $ if cykStr grammar expr then "YES" else "NO"