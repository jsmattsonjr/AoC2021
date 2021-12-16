import AoC2021
import Data.List
import Data.List.Split
import Options.Applicative

main = do
  opts <- execParser (optionsParser "Day 4: Giant Squid")
  str <- getInput (file opts)
  print $ solve (part opts) $ parse str

type Board = [[Int]]

parse :: String -> ([Int], [Board])
parse str = (nums, boards)
  where
    input = filter (/= "") $ lines str
    nums = parseNumbers $ head input
    parseNumbers = map read . splitOn ","
    boards = map parseBoard $ chunksOf 5 $ tail input
    parseBoard = map (map read . words)

solve part (nums, boards) = score (play part nums [] boards)
  where
    score (board, called) = sumUnmarked * head called
      where
        sumUnmarked = sum $ filter (`notElem` called) $ concat board

play part nums@(num:more) called boards =
  case firstWinner called boards of
    Nothing -> play part more (num : called) boards
    Just board ->
      if part == 1 || length boards == 1
        then (board, called)
        else play part nums called (delete board boards)

firstWinner _ [] = Nothing
firstWinner called (board:boards) =
  if any (completeRow called) board ||
     any (completeRow called) (transpose board)
    then Just board
    else firstWinner called boards
  where
    completeRow called row = all (`elem` called) row
