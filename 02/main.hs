--

type Aim = Int
type Horizontal = Int
type Depth = Int

type Instruction = (String, Int)

-- PART 01

type Location = (Horizontal, Depth)

move :: Location -> Instruction -> Location
move (horiz, x) ("forward", val) = (horiz + val, x)
move (x, depth) ("up", val) = (x, depth - val)
move (x, depth) ("down", val) = (x, depth + val)
move _ _ = undefined -- to shut up linter

getLoc :: [Instruction] -> Location
getLoc = foldl move (0, 0)

-- PART 02

type Location2 = (Horizontal, Depth, Aim)

move2 :: Location2 -> Instruction -> Location2
move2 (horiz, depth, aim) ("forward", val) = (horiz + val, newDepth, aim)
  where newDepth = depth + aim * val
move2 (horiz, depth, aim) ("up", val) = (horiz, depth, aim - val)
move2 (horiz, depth, aim) ("down", val) = (horiz, depth, aim + val)
move2 _ _ = undefined

getLoc2 :: [Instruction] -> Location2
getLoc2 = foldl move2 (0, 0, 0)

parse :: String -> Instruction
parse s = (ins, val)
  where split = words s
        ins = head split
        val = (read :: String -> Int) $ last split

helper :: Location2 -> Location
helper (x, y, _) = (x, y)

main :: IO ()
main = do
    file <- readFile "input.txt"
    let filelines = lines file
    let instructions = map parse filelines
    --let location = getLoc instructions
    let location = helper $ getLoc2 instructions
    print $ uncurry (*) location 
    -- putStrLn $ show $ fst location * snd location
