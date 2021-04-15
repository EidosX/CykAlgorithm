module Automata.Types where

import Data.List (intercalate)

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

-- For debug purposes
instance Show Symbol where show (Symbol s) = s
                           show Epsilon    = "ε"

instance Show Transition where
  show t = o <> " -> " <> d <> " (" <> s <> ", " <> st <> "/" <> nst <> ")"
    where State o        = origin t
          State d        = dest   t
          s              = show $ symbol t
          StackSymbol st = stackTop t
          nst            = intercalate "." . map (\(StackSymbol a) -> a) $ newStackTop t

instance Show Automata where
  show Automata {transitions, finalStates} =
    "Transitions: \n" <> unlines (("  " ++) . show <$> transitions) <>
    "Final States: " <> intercalate ", " ((\(State a) -> a) <$> finalStates)