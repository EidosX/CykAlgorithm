module Automata.Types where

import Data.List (intercalate)

data Automata = Automata {
  transitions   :: [Transition],
  finalStates   :: [State],
  entryState    :: State,
  entryStackSym :: StackSymbol
}

data Transition = Transition {
  origin      :: State,
  dest        :: State,
  symbol      :: Symbol,
  stackTop    :: StackSymbol,
  newStackTop :: [StackSymbol]
}



data State = State String
  deriving (Eq, Show)

data Symbol = Symbol String | Epsilon
  deriving Eq

data StackSymbol = StackSymbol String
  deriving (Eq, Show)


type Stack = [StackSymbol]

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
  show Automata {entryStackSym, entryState, transitions, finalStates} =
    "Transitions: \n" <> unlines (("  " ++) . show <$> transitions) <>
    "Entry Stack State: " <> entryStackSym' <> "\n" <>
    "Entry State: " <> entryState' <> "\n" <>
    "Final States: " <> intercalate ", " ((\(State a) -> a) <$> finalStates) <> "\n"
    where State entryState' = entryState
          StackSymbol entryStackSym' = entryStackSym