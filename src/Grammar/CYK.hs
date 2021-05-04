module Grammar.CYK where

import Grammar.Types
import Data.List (nub)

cykStr :: Grammar -> [Char] -> Bool
cykStr g = cyk g . map (\c -> Terminal [c])

-- This function assumes a grammar in chomsky normal form
cyk :: Grammar -> [Terminal] -> Bool
cyk g [] = any ((== []) . to) $ rules g
cyk g str = entryVar g `elem` cyk' g str (length str - 1) 0

cyk' :: Grammar -> [Terminal] -> Int -> Int -> [Var]
cyk' g str j i
  | j == 0 = nub . map from . filter ((== [Right $ str !! i]) . to) $ rules g
  | otherwise = do
      splitIndex <- [1..j]
      leftVar <- cyk' g str (splitIndex - 1) i
      rightVar <- cyk' g str (j - splitIndex) (i + splitIndex)
      map from . filter ((== [Left leftVar, Left rightVar]) . to) $ rules g