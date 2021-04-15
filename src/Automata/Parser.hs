module Automata.Parser (parseAutomata) where

import Automata.Types
import Data.List.Split (wordsBy)

parseAutomata :: String -> Automata
parseAutomata str = Automata {
  finalStates   = parseFinalStates . last $ lines str,
  entryState    = origin   $ head transitions,
  entryStackSym = stackTop $ head transitions,
  transitions
} where transitions = parseTransition <$> init (lines str)

parseSymbol :: String -> Symbol
parseSymbol "%" = Epsilon
parseSymbol  s  = Symbol s

-- Parses strings like "A.B.ZZ"
parseStackSymList :: String -> [StackSymbol]
parseStackSymList "%" = []
parseStackSymList  s  = StackSymbol <$> wordsBy (== '.') s

-- Parses strings like "A 2 55 1"
parseFinalStates :: String -> [State]
parseFinalStates = map State . tail . words

parseTransition :: String -> Transition
parseTransition s = Transition (State origin) (State dest)
                               (parseSymbol symbol)
                               (StackSymbol stackTop) 
                               (parseStackSymList newStackTop)
  where [origin, symbol, stackTop, newStackTop, dest] = words s