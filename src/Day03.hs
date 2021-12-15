import AoC2021
import Data.Char
import Data.List
import Options.Applicative

main = do
  opts <- execParser (optionsParser "Day 3: Binary Diagnostic")
  str <- getInput (file opts)
  print $ solve (part opts) $ parse str

parse str = map (map digitToInt) $ words str

solve part report =
  case part of
    1 -> gamma * epsilon
      where gamma = calcRate (>) report
            epsilon = calcRate (<) report
    2 -> oxygen * co2
      where oxygen = calcRating (>=) report 0
            co2 = calcRating (<) report 0

calcRate f = toInt 2 . map (choose f) . transpose

choose f bits = fromEnum $ length ones `f` length zeros
  where
    (ones, zeros) = partition (== 1) bits

calcRating f [line] _ = toInt 2 line
calcRating f lines pos = calcRating f matching (pos + 1)
  where
    match = choose f (transpose lines !! pos)
    matching = filter matchesOn lines
    matchesOn line = line !! pos == match
