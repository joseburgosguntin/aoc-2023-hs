{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import Data.Attoparsec.ByteString.Char8 (isAlpha_ascii)
import Data.Attoparsec.Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import Prelude hiding (Left, Right)

data Direction = Left | Right
    deriving (Show)

parseDirection :: Parser Direction
parseDirection = do
    fromChar <$> satisfy validLeftRight
  where
    validLeftRight :: Char -> Bool
    validLeftRight = (`elem` leftRight)
    leftRight :: String
    leftRight = "LR"

type Nodes = Map Text (Text, Text)

parseNode :: Parser (Text, (Text, Text))
parseNode = do
    key <- takeWhile1 isAlpha_ascii
    string " = ("
    left <- takeWhile1 isAlpha_ascii
    string ", "
    right <- takeWhile1 isAlpha_ascii
    string ")"
    return (key, (left, right))

fromChar :: Char -> Direction
fromChar = \case
    'L' -> Left
    'R' -> Right

parseDirectionsAndNodes :: Parser ([Direction], Nodes)
parseDirectionsAndNodes = do
    directions <- many1 parseDirection
    string "\n\n"
    nodes <- parseNode `sepBy` char '\n'
    return (directions, Map.fromList nodes)

choseNode :: Direction -> Nodes -> Text -> Text
choseNode direction nodes key = case direction of
    Left -> left
    Right -> right
  where
    (left, right) = fromJust $ Map.lookup key nodes

findZZZ :: [Direction] -> Nodes -> Int
findZZZ directions nodes = go "AAA" 0 directions
  where
    go :: Text -> Int -> [Direction] -> Int
    go key steps (d : ds)
        | key == "ZZZ" = steps
        | otherwise =
            go (choseNode d nodes key) (steps + 1) ds

part1 :: Parser Int
part1 = do
    (directions, nodes) <- parseDirectionsAndNodes
    return $ findZZZ (cycle directions) nodes

findZEndings :: [Direction] -> Nodes -> Integer
findZEndings directions nodes = go aEndings 0 directions
  where
    checkEnding :: Char -> Text -> Bool
    checkEnding c = (c ==) . Text.last
    aEndings :: [Text]
    aEndings = filter (checkEnding 'A') $ Map.keys nodes
    next :: Direction -> [Text] -> [Text]
    next d = map (choseNode d nodes)
    go :: [Text] -> Integer -> [Direction] -> Integer
    go keys steps (d : ds)
        | all (checkEnding 'Z') keys = steps
        | otherwise = go (next d keys) (steps + 1) ds

part2 :: Parser Integer
part2 = do
    (directions, nodes) <- parseDirectionsAndNodes
    return $ findZEndings (cycle directions) nodes
