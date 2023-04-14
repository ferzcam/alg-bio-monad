module PatternCount
( patternCount,
  tails
) where

tails :: String -> [String]
tails [] = []
tails (x:xs) = (x:xs) : tails xs

patternCount :: String -> String -> Int
patternCount text pattern = length $ filter (== pattern) $ map (take size) $ tails text
  where
    size = length pattern


