module Grammar.Types where

data Variable = Variable String
  deriving Eq

data Terminal = Terminal String
  deriving Eq

data Transition = Transition {
  from :: Variable,
  to   :: [Either Variable Terminal]
}

data Grammar = Grammar {
  transitions   :: [Transition],
  entryVariable :: Variable
}