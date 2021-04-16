module Grammar.Parser (parseGrammar) where

import Grammar.Types
import Data.List.Split (wordsBy)

parseGrammar :: String -> Grammar
parseGrammar str = Grammar {
  transitions = parseTransition variables <$> lines',
  entryVariable = head variables
} where variables = Variable . head . words <$> lines'
        lines' = filter (/= "") $ lines str

parseTransition :: [Variable] -> String -> Transition
parseTransition vars str = Transition (Variable from') (parseSeq to') 
  where 
    [from', to'] = words str
    parseSeq "%"  = []
    parseSeq str' = f <$> wordsBy (== '.') str'
      where f v | Variable v `elem` vars = Left  $ Variable v
            f t                          = Right $ Terminal t