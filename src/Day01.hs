module Main where

import AoC2021
import Options.Applicative

main = do
  opts <- execParser (optionsParser "Day 1: Sonar Sweep")
  str <- getInput (file opts)
  print $ solve (part opts) $ parse str

parse :: String -> [Int]
parse = map read . words

solve part depths = sum $ map fromEnum increases
  where
    increases = zipWith isDeeper depths (drop delta depths)
    isDeeper x y = y > x
    delta =
      case part of
        1 -> 1
        2 -> 3
