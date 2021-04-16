module Grammar.CNFMaker where

import Grammar.Types
import Grammar.Grammar

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
removeNonSolitaryTerminals g' = g' {rules = terminalRules ++ map f (rules g')}
  where f r | length (to r) <= 1 = r
            | otherwise          = r {to = terminalReplacer <$> to r}
        terminalRules = (\t -> Rule (terminalToVar t) [Right t]) <$> terminals g'
        terminalReplacer (Left v)  = Left v
        terminalReplacer (Right t) = Left $ terminalToVar t
        terminalToVar (Terminal t) = createNewVar (vars g') ("X_" ++ t)

removeLongRHS :: Grammar -> Grammar
removeLongRHS _g' = undefined

-- Makes sure not to have name conflict
createNewVar :: [Var] -> String -> Var
createNewVar vs s | Var s `elem` vs = createNewVar vs (s ++ "_0")
                  | otherwise       = Var s