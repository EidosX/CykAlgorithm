module Grammar.CNFMaker where

import Grammar.Types
import Grammar.Grammar
import Data.Either (partitionEithers)

makeCNF :: Grammar -> Grammar
makeCNF = undefined
        . undefined
        . removeLongRHS
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
        f r | length (to r) <= 1 = [r]
        f r = undefined

removeLongRHS :: Grammar -> Grammar
removeLongRHS g' = undefined

-- Makes sure not to have name conflict
createNewVar :: [Var] -> String -> Var
createNewVar vs s | Var s `elem` vs = createNewVar vs (s ++ "_0")
                  | otherwise       = Var s