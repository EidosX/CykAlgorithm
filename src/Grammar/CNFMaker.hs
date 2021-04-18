module Grammar.CNFMaker where

import Grammar.Types
import Grammar.Grammar

makeCNF :: Grammar -> Grammar
makeCNF = removeUnitRules
        . removeEpsilons
        . removeLongRHS
        . removeNonSolitaryTerminals
        . removeEntrySymbolFromRHS

removeEntrySymbolFromRHS :: Grammar -> Grammar
removeEntrySymbolFromRHS g = Grammar (newTrans : rules g) newEntry
  where Var oldEntryStr = entryVar g
        newTrans = Rule newEntry [Left $ entryVar g]
        newEntry = createNewVar (vars g) oldEntryStr

removeNonSolitaryTerminals :: Grammar -> Grammar
removeNonSolitaryTerminals g = g {rules = terminalRules ++ map f (rules g)}
  where f r | length (to r) <= 1 = r
            | otherwise          = r {to = terminalReplacer <$> to r}
        terminalRules = (\t -> Rule (terminalToVar t) [Right t]) <$> terminals g
        terminalReplacer (Left v)  = Left v
        terminalReplacer (Right t) = Left $ terminalToVar t
        terminalToVar (Terminal t) = createNewVar (vars g) ("X_" ++ t)

-- This function pre-supposes vars and terminals are separated in each rule
removeLongRHS :: Grammar -> Grammar
removeLongRHS g' | all (<= 2) (length . to <$> rules g') = g'
                 | otherwise = removeLongRHS g' {rules = f $ rules g'} where
  f [] = []
  f (r:rs) | length (to r) > 2 = headRule : newRule : rs
           | otherwise = r : f rs
    where newVar = createNewVar (vars g') "Q"
          newRule = Rule newVar (tail $ to r)
          headRule = Rule (from r) [head $ to r, Left newVar]

removeEpsilons :: Grammar -> Grammar
removeEpsilons g = g {rules=filter (\r -> to r /= [] || from r == entryVar g) rules'}
  where rules' = rules g >>= \r -> Rule (from r) <$> nullPs (to r)
        nullPs (Left e : xs) | nullable g e = nullPs xs ++ ((Left e :) <$> nullPs xs)
        nullPs (x:xs) = (x :) <$> nullPs xs
        nullPs [] = [[]]
        -- nullPowerset example with A and B nullables (not in order): 
        --   nullPs aSAxB = [aSAxB, aSAx, aSxB, aSx]

removeUnitRules :: Grammar -> Grammar
removeUnitRules = until (all isNotUnitRule . rules) (\g -> g {rules=rules g >>= f g})
  where f g (Rule from [Left var]) = [Rule from to' | Rule from' to' <- rules g, from' == var]
        f _ r = [r]
        isNotUnitRule (Rule _ [Left _]) = False; isNotUnitRule _ = True


-- Makes sure not to have name conflict
createNewVar :: [Var] -> String -> Var
createNewVar vs s | Var s `elem` vs = createNewVar vs (s ++ "_0")
                  | otherwise       = Var s