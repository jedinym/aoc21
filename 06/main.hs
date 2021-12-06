--
import Data.List.Split (splitOn)

type Cnt = Int
type Val = Int

data Fish = School Val Cnt
  deriving (Show)

parseFish :: String -> [Fish]
parseFish s = map (mkfish . parse) $ splitOn "," s
  where parse = read :: String -> Int
        mkfish n = School n 1

age :: Fish -> Fish
age (School val cnt) = School (newVal val) cnt
  where newVal 0 = 6
        newVal n = n - 1

accOffSpring :: Fish -> Fish -> Fish
accOffSpring (School _ acc) (School val cnt)
  | val == 0 = School 8 (acc + cnt)
  | otherwise = School 8 acc

countOffspring :: [Fish] -> Fish
countOffspring = foldl accOffSpring (School 8 0)

nextGeneration :: [Fish] -> [Fish]
nextGeneration fish = offspring : aged 
  where aged = map age fish
        offspring = countOffspring fish

countFish :: [Fish] -> Integer
countFish = foldl summer 0
  where
    summer :: Integer -> Fish -> Integer
    summer acc (School _ cnt) = acc + fromIntegral cnt

iterateFish :: [Fish] -> Int -> [Fish]
iterateFish fish n = iterate nextGeneration fish !! n

main :: IO ()
main = do
    file <- readFile "input.txt"
    let fish = parseFish $ (head . lines) file
    let iterated = iterateFish fish 256 
    print $ countFish iterated 
