module Automata.Automata where

import Automata.Types 
import Data.List (nub)
import Data.Maybe (listToMaybe)

states :: Automata -> [State]
states a = nub $ origins ++ dests
  where origins = origin <$> transitions a
        dests   = dest   <$> transitions a

-- Returns all symbols (no epsilon)
symbols :: Automata -> [Symbol]
symbols = filter (/= Epsilon) . nub . map symbol . transitions

stackSymbols :: Automata -> [StackSymbol]
stackSymbols a = nub $ newTops ++ tops
  where tops    = stackTop <$> transitions a
        newTops = transitions a >>= newStackTop

-- This is the δ(q, a, X) function
deltaTransitions :: Automata -> State -> Symbol -> StackSymbol -> [Transition]
deltaTransitions a orig sym stTop = 
  filter (\(Transition {origin, symbol, stackTop}) -> 
              origin   == orig &&
              symbol   == sym  &&
              stackTop == stTop
         ) $ transitions a

-- Use this with deterministic automatas guaranteed to have 0 or 1 transition
transition :: Automata -> State -> Symbol -> StackSymbol -> Maybe Transition
transition a st s ss = listToMaybe $ deltaTransitions a st s ss

isDeterministic :: Automata -> Bool
isDeterministic a = and [
    let f symbol = deltaTransitions a st symbol stSym in
    length (f sym ++ f Epsilon) <= 1 -- δ(q, a, X) + δ(q, ε, X)
    | st    <- states a, 
      sym   <- symbols a, 
      stSym <- stackSymbols a
  ]

-- Only works if all symbols consist of one character!
accepts :: Automata -> [Char] -> Bool
accepts a = acceptsSymbols a . map (\c -> Symbol [c])

-- Only works with deterministic automatas!
acceptsSymbols :: Automata -> [Symbol] -> Bool
acceptsSymbols a str' = f str' (entryState a) [entryStackSym a] where
  f :: [Symbol] -> State -> Stack -> Bool

  -- We ignore epsilons in the string
  f (Epsilon : str) state xs = f str state xs

  -- If string is not finished and current symbol has a transition, we take it. 
  f (s : str) state (x : xs) | Just t <- transition a state s x =
    f str (dest t) (newStackTop t ++ xs)
  
  -- If string is finished and we're on a final state, we've won!
  f [] state _ | state `elem` finalStates a = True

  -- If there is an epsilon transition, we take it.
  f str state (x : xs) | Just t <- transition a state Epsilon x =
    f str (dest t) (newStackTop t ++ xs)
  
  -- Finally, if nothing worked, we've lost.
  f _ _ _ = False