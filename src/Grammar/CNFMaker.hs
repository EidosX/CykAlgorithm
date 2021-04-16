module Grammar.CNFMaker (makeCNF) where

import Grammar.Types
import Grammar.Grammar

makeCNF :: Grammar -> Grammar
makeCNF = undefined
        . undefined
        . undefined
        . removeNonSolitaryTerminals
        . removeEntrySymbolFromRHS

removeEntrySymbolFromRHS :: Grammar -> Grammar
removeEntrySymbolFromRHS g = Grammar (newTrans : rules g) newEntry
  where Var oldEntryStr = entryVar g
        newTrans = Rule newEntry [Left $ entryVar g]
        newEntry = createNewVar (vars g) oldEntryStr

removeNonSolitaryTerminals :: Grammar -> Grammar
removeNonSolitaryTerminals g' = g' {rules = rules g' >>= f}
  where f :: Rule -> [Rule]
        f t | length (to t) <= 1 = [t]
            -- | otherwise = -- TODO

-- Makes sure not to have name conflict
createNewVar :: [Var] -> String -> Var
createNewVar vs s | Var s `elem` vs = createNewVar vs (s ++ "_0")
                  | otherwise       = Var s