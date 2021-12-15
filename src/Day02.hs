module Main where

import AoC2021
import Data.List.Split
import Options.Applicative

type Command = (Directive, Int)

type State = (Int, Int, Int)

data Directive
  = Forward
  | Down
  | Up
  deriving (Eq)

main = do
  opts <- execParser (optionsParser "Day 2: Dive!")
  str <- getInput (file opts)
  print $ solve (part opts) $ parse str

parse :: String -> [Command]
parse str = map toCommand $ chunksOf 2 $ words str
  where
    toCommand [dir, dist] = (toDirective dir, read dist)
    toDirective "forward" = Forward
    toDirective "down" = Down
    toDirective "up" = Up

solve part commands = horiz * depth
  where
    (horiz, depth, _) = foldl (processCommand part) (0, 0, 0) commands

processCommand part (horiz, depth, aim) (dir, dist) =
  case part of
    1 ->
      case dir of
        Forward -> (horiz + dist, depth, aim)
        Down -> (horiz, depth + dist, aim)
        Up -> (horiz, depth - dist, aim)
    2 ->
      case dir of
        Forward -> (horiz + dist, depth + aim * dist, aim)
        Down -> (horiz, depth, aim + dist)
        Up -> (horiz, depth, aim - dist)
