import Data.List

list :: [Int]
list = [3, 4, 9, 14, 15, 19, 28, 37, 47, 50, 54, 56, 59, 61, 70, 73, 78, 81, 92, 95, 97, 99]

maxSumsRest :: (Num a, Ord a) => [a] -> Bool
maxSumsRest [] = False
maxSumsRest xs = head max == sum rest
  where
    (max, rest) = partition (== maximum xs) xs

main :: IO ()
main = print $ length $ filter maxSumsRest $ subsequences list