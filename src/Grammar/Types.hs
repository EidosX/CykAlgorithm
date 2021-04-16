module Grammar.Types where

data Var = Var String
  deriving (Eq, Show)

data Terminal = Terminal String
  deriving (Eq, Show)

data Rule = Rule {
  from :: Var,
  to   :: [Either Var Terminal]
}

data Grammar = Grammar {
  rules    :: [Rule],
  entryVar :: Var
}