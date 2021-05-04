module Grammar.CYK where

import Grammar.Types
import Data.Function.Memoize (memoize2)
import Data.List (nub)

-- This function assumes all terminals in grammar are a single char
cykStr :: Grammar -> [Char] -> Bool
cykStr g = cyk g . map (\c -> Terminal [c])

-- This function assumes a grammar in chomsky normal form
cyk :: Grammar -> [Terminal] -> Bool
cyk g [] = any ((== []) . to) $ rules g
cyk g str = entryVar g `elem` cyk' (length str - 1) 0 where
  cyk' = memoize2 f
  f j i | j == 0 = nub . map from . filter ((== [Right $ str !! i]) . to) $ rules g
        | otherwise = do
            splitIndex <- [1..j]
            leftVar <- cyk' (splitIndex - 1) i
            rightVar <- cyk' (j - splitIndex) (i + splitIndex)
            map from . filter ((== [Left leftVar, Left rightVar]) . to) $ rules g