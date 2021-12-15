module AoC2021 where

import Options.Applicative

data Options =
  Options
    { part :: Int
    , file :: String
    }

parsePart :: Parser Int
parsePart = option auto (long "part" <> short 'p')

parseFile :: Parser String
parseFile = option str (long "file" <> short 'f' <> value "-")

programOptions :: Parser Options
programOptions = Options <$> parsePart <*> parseFile

optionsParser :: String -> ParserInfo Options
optionsParser dayDesc =
  info
    (helper <*> programOptions)
    (fullDesc <> progDesc ("Advent of Code 2021 -- " ++ dayDesc))

getInput :: String -> IO String
getInput "-" = getContents
getInput file = readFile file

toInt base digits = snd $ foldr toDecimal (1, 0) digits
  where
    toDecimal digit (place, accum) = (place * base, accum + digit * place)
