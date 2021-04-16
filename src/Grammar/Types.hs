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

-- For debugging

instance Show Rule where
  show (Rule (Var from) ts) = from ++ " -> " ++ unwords (map showSym ts)
    where showSym (Right x) = "(" ++ show x ++ ")"
          showSym (Left x) = "(" ++ show x ++ ")"

instance Show Grammar where
  show Grammar {rules, entryVar} = "Entry: " ++ show entryVar ++ "\nRules:\n" ++
    unlines (map (("  " ++) . show) rules)