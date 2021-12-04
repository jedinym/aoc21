--
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Bits

type One = Int
type Zero = Int
type Count = (One, Zero)

parse :: String -> Int
parse = read :: String -> Int

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

binToInt' :: [Int] -> Int -> Int
binToInt' [] _ = 0
binToInt' (x:xs) n = (2^n) * x + binToInt' xs (n+1)

binToInt :: [Int] -> Int
binToInt x = binToInt' reversed 0
    where reversed = reverse x

count :: Count -> Int -> Count
count (one, zero) n
 | n == 0 = (one, zero + 1)
 | otherwise = (one + 1, zero)

f :: Int -> Int -> Int
f n = (.&. n)

gamma :: Count -> Int
gamma (one, zero)
 | one > zero = 1
 | otherwise = 0

epsilon :: Count -> Int
epsilon (one, zero)
 | one > zero = 0
 | otherwise = 1

countAll :: [Int] -> Int -> [Count]
countAll _ (-1) = []
countAll nums coeff = cnt (map (f (2^coeff)) nums) : countAll nums (coeff - 1)
  where cnt = foldl count (0, 0)

main :: IO ()
main = do
    file <- readFile "input.txt"
    let filelines = lines file
    let nums = map toDec filelines
    let counted = countAll nums 11 
    let gammaRate = binToInt $ map gamma counted
    let epsilonRate = binToInt $ map epsilon counted
    print $ gammaRate * epsilonRate