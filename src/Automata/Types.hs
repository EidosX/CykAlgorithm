module Automata.Types where

data State = State String
  deriving Eq

data Symbol = Symbol String | Epsilon
  deriving Eq

data StackSymbol = StackSymbol String
  deriving Eq

data Transition = Transition {
  origin      :: State,
  dest        :: State,
  symbol      :: Symbol,
  stackTop    :: StackSymbol,
  newStackTop :: [StackSymbol]
}

data Automata = Automata {
  transitions :: [Transition],
  finalStates :: [State]
}