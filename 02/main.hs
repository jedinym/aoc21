--

type Location = (Int, Int)
type Instruction = (String, Int)


parse :: String -> Instruction
parse s = (ins, val)
  where split = words s
        ins = head split
        val = (read :: String -> Int) $ last split


move :: Location -> Instruction -> Location 
move (horiz, x) ("forward", val) = (horiz + val, x)
move (x, depth) ("up", val) = (x, depth - val)
move (x, depth) ("down", val) = (x, depth + val)


getLoc :: [Instruction] -> Location
getLoc ins = foldl (move) (0, 0) ins


main :: IO ()
main = do
    file <- readFile "input.txt"
    let filelines = lines file
    let instructions = map parse filelines
    let location = getLoc instructions
    putStrLn $ show $ (fst location) * (snd location)
