{-# LANGUAGE OverloadedStrings #-}

module Day6 (part1, part2) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text (Text, pack)

part1 :: Parser Int
part1 = do
    pairs <-
        zip
            <$> intsParser "Time"
            <* char '\n'
            <*> intsParser "Distance"
    return $ product $ length . wins <$> pairs

part2 :: Parser Int
part2 = do
    pair <-
        (,)
            <$> intParser "Time"
            <* char '\n'
            <*> intParser "Distance"
    return $ length $ wins pair

intsParser :: Text -> Parser [Int]
intsParser tag = do
    string (tag <> ":") *> many1 space
    decimal `sepBy1` many1 space

intParser :: Text -> Parser Int
intParser tag = do
    string (tag <> ":") *> many1 space
    intPieces <- many1 digit `sepBy` many1 space
    return $ read $ concat intPieces

wins :: (Int, Int) -> [Int]
wins (time, distance) =
    filter beats [0, 1 .. time]
  where
    beats pressed = distance < (time - pressed) * pressed
