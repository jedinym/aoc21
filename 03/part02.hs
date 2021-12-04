--

import Debug.Trace (trace)
import Data.Char (digitToInt)
import Data.List (foldl')

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

data Criterion = One | Zero
    deriving (Show)

type Count = (Int, Int)

parse :: String -> Int
parse = read :: String -> Int

count :: Count -> Char -> Count
count (one, zero) n
 | n == '0' = (one, zero + 1)
 | otherwise = (one + 1, zero)

countAll :: [Char] -> Count
countAll = foldl count (0, 0)

critO2 :: Count -> Criterion
critO2 (one, zero)
 | one > zero = One
 | one < zero = Zero
 | otherwise = One

critCO2 :: Count -> Criterion
critCO2 (one, zero)
 | one > zero = Zero
 | one < zero = One
 | otherwise = Zero 

g :: Criterion -> Char -> Bool
g One '1' = True
g Zero '0' = True
g _ _ = False 

-- position maxPosition records 
f' :: Int -> Int -> [String] -> (Count -> Criterion) -> [String]
f' pos maxPos records critFunc 
 | pos == maxPos = records
 | otherwise = if length filtered == 1 then filtered
                else f' (pos+1) maxPos filtered critFunc 
  where selector = (!!pos)
        selected = map selector records
        counted = countAll selected
        crit = critFunc counted
        predicate = g crit
        filtered = filter (predicate . selector) records


getRating :: [String] -> (Count -> Criterion) -> String
getRating records critFunc = head $ f' 0 (length $ head records) records critFunc

main :: IO ()
main = do
    file <- readFile "input.txt"
    let records = lines file
    let co2rating = getRating records critCO2
    let o2rating = getRating records critO2
    print (toDec co2rating * toDec o2rating)
