--
import Data.List.Split (splitOn)

type Fish = Int

parseFish :: String -> [Fish]
parseFish s = map parse $ splitOn "," s 
  where parse = read :: String -> Fish 


iterateFish :: [Fish] -> Int -> [Fish]
iterateFish fish n = iterate nextDay fish !! n

ageAndAdvance :: Fish -> [Fish]
ageAndAdvance 0 = [6, 8]
ageAndAdvance n = [n - 1]

nextDay :: [Fish] -> [Fish]
nextDay = concatMap ageAndAdvance 

main :: IO ()
main = do
    file <- readFile "test.txt"
    let fish = parseFish $ (head . lines) file
    print $ iterateFish fish 256
