module Automata.Automata where

import Automata.Types
import Data.List

states :: Automata -> [State]
states a = nub $ origins ++ dests
  where origins = origin <$> transitions a
        dests   = dest   <$> transitions a

symbols :: Automata -> [Symbol]
symbols = nub . map symbol . transitions

stackSymbols :: Automata -> [StackSymbol]
stackSymbols a = nub $ newTops ++ tops
  where tops    = stackTop <$> transitions a
        newTops = transitions a >>= newStackTop

deltaTransitions :: Automata -> State -> Symbol -> StackSymbol -> [Transition]
deltaTransitions a orig sym stTop = 
  filter (\(Transition {origin, symbol, stackTop}) -> 
              origin   == orig &&
              symbol   == sym  &&
              stackTop == stTop
         ) $ transitions a
