--
import Data.List.Split (splitOn)
import Data.Maybe (isJust, fromJust)
import Debug.Trace (trace)

data Square = Marked Int | Unmarked Int
    deriving (Show)

type Board = [[Square]]
type Numbers = [Int]

-- parsing --
parseNum :: String -> Square
parseNum s = Unmarked num
  where num = (read :: String -> Int) s

parseBoardLine :: String -> [Square]
parseBoardLine ln = map parseNum nums
  where nums = words ln

parseBoard :: [String] -> Board
parseBoard = map parseBoardLine

parseBoards :: [String] -> [Board]
parseBoards [] = []
parseBoards lns = board : parseBoards restLines
  where board = parseBoard $ take 5 lns
        restLines = drop 5 lns

--parsing-end--



markNumber :: Int -> Board -> Board
markNumber _ [] = []
markNumber n (row:rows) = map mark row : markNumber n rows
  where mark sq@(Unmarked val) = if val == n then Marked val else sq
        mark sq@(Marked val) = sq

isWinning :: [Square] -> Bool
isWinning = all isMarked 
  where isMarked (Marked _) = True 
        isMarked (Unmarked _) = False

getCol :: Int -> Board -> [Square]
getCol rowNum board = [board !! colNum !! rowNum | colNum <- [0..4]]

getCols :: Board -> [[Square]]
getCols board = [getCol rowNum board | rowNum <- [0..4]]

boardIsWinning :: Board -> Bool
boardIsWinning board = any isWinning rows || any isWinning cols
  where cols = getCols board
        rows = board

getWinningBoard :: [Board] -> Maybe Board
getWinningBoard [] = Nothing 
getWinningBoard (board:boards) 
 | boardIsWinning board = Just board 
 | otherwise = getWinningBoard boards

playTurn :: Int -> [Board] -> [Board]
playTurn n = map play 
  where play = markNumber n 

findFirstWinning :: Numbers -> [Board] -> (Board, Int)
findFirstWinning (drawn:nums) boards
 | isJust winning = (fromJust winning, drawn)
 | otherwise = findFirstWinning nums played 
  where played = playTurn drawn boards 
        winning = getWinningBoard played 
findFirstWinning _ _ = undefined  -- to shut up linter

findLastWinning :: Numbers -> [Board] -> (Board, Int)
findLastWinning (drawn:nums) [board]
 | boardIsWinning playedBoard = (playedBoard, drawn)
 | otherwise = findLastWinning nums [board]
  where played = playTurn drawn [board]
        playedBoard = head played
findLastWinning (drawn:nums) boards = findLastWinning nums notWinning
  where played = playTurn drawn boards
        notWinning = filter (not . boardIsWinning) played
findLastWinning _ _ = undefined -- to shut up linter


sumUnmarked :: Board -> Int
sumUnmarked = sum . map sumUnmarkedLine
  where sumUnmarkedLine = foldl f 0 
        f acc (Unmarked val) = acc + val
        f acc (Marked _) = acc 


getScore :: (Board, Int) -> Int
getScore (b, n) = sumUnmarked b * n


main :: IO ()
main = do
    file <- readFile "input.txt"
    let filelines = filter (/="") $ lines file
    let drawNumbers = map (read :: String -> Int) $ splitOn "," $ head filelines
    let bingoBoardLines = drop 1 filelines
    let boards = parseBoards bingoBoardLines
    print $ getScore $ findFirstWinning drawNumbers boards 
    print $ getScore $ findLastWinning drawNumbers boards 
