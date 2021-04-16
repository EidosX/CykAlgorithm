module Grammar.Parser (parseGrammar) where

import Grammar.Types
import Data.List.Split (wordsBy)

parseGrammar :: String -> Grammar
parseGrammar str = Grammar {
  rules = parseRule vars <$> lines',
  entryVar = head vars
} where vars   = Var . head . words <$> lines'
        lines' = filter (/= "") $ lines str

parseRule :: [Var] -> String -> Rule
parseRule vars str = Rule (Var from') (parseSeq to') 
  where 
    [from', to'] = words str
    parseSeq "%"  = []
    parseSeq str' = f <$> wordsBy (== '.') str'
      where f v | Var v `elem` vars = Left  $ Var v
            f t                     = Right $ Terminal t