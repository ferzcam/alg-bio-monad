module FrequentWords
  ( frequentWords
  ) where

import PatternCount (tails)
import Data.List (groupBy, sort)

frequentWords :: String -> Int -> [String]
frequentWords text k = map head $ filter ((== maxCount) . length)  groups
  where
    words = map (take k) $ tails text
    groups = groupBy (==) $ sort words
    maxCount = maximum $ map length groups
