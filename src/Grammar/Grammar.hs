module Grammar.Grammar where

import Grammar.Types
import Data.List (nub)
import Data.Either (lefts, rights)

isCNF :: Grammar -> Bool
isCNF Grammar {transitions, entryVariable} = all valid transitions where 
  valid (Transition _ [Right _]        )                                   = True
  valid (Transition _ [Left v, Left v']) | entryVariable `notElem` [v, v'] = True
  valid (Transition v []               ) | v == entryVariable              = True
  valid _ = False

variables :: Grammar -> [Variable]
variables g = nub $ entryVariable g 
                  : map from (transitions g) 
                  ++ (transitions g >>= lefts . to)

terminals :: Grammar -> [Terminal]
terminals g = transitions g >>= rights . to