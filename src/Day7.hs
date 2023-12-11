{-# LANGUAGE OverloadedStrings #-}

module Day7 (part1, part2, totalWinnings, Hand (..), Label (..)) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char (digitToInt, isDigit)
import Data.List (partition, sortBy, sortOn)
import Data.Text (Text, group, pack)
import Text.Read (Lexeme (String))

part1 :: Parser Int
part1 = totalWinnings <$> handsWithBidsParser

part2 :: Parser Int
part2 = undefined

newtype Label = Label Char
    deriving (Eq, Show)

instance Ord Label where
    Label a <= Label b = case (a, b) of
        (_, 'A') -> True
        (a, 'K') | a /= 'A' -> True
        (a, 'Q') | a /= 'A' && a /= 'K' -> True
        (a, 'J') | a /= 'A' && a /= 'K' && a /= 'Q' -> True
        (a, 'T') | a /= 'A' && a /= 'K' && a /= 'Q' && a /= 'T' -> True
        (a, b) | isDigit a && isDigit b -> digitToInt a <= digitToInt b
        _ -> False

-- is a 5 element array
-- tested Ord
newtype Hand = Hand [Label]
    deriving (Eq, Ord, Show)

-- tested Ord
data Type
    = HighCard
    | OnePair
    | TwoPair
    | ThreeOfAKind
    | FullHouse
    | FourOfAKind
    | FiveOfAKind -- strongest
    deriving (Show, Eq, Ord)

labelToChar (Label char) = char

-- tested ThreeOfAKind
typeFromHand :: Hand -> Type
typeFromHand (Hand hand) = case grouped of
    [_] -> FiveOfAKind
    [_, _] | hasNSame 4 -> FourOfAKind
    [_, _] -> FullHouse
    [_, _, _] | hasNSame 3 -> ThreeOfAKind
    [_, _, _] -> TwoPair
    [_, _, _, _] -> OnePair -- pidgeon hole principle
    _ -> HighCard
  where
    grouped = groupBy2 (==) hand
    hasNSame n = any ((== n) . length) grouped
    groupBy2 :: (a -> a -> Bool) -> [a] -> [[a]]
    groupBy2 = go []
      where
        go acc comp [] = acc
        go acc comp (h : t) =
            let (hs, nohs) = partition (comp h) t
             in go ((h : hs) : acc) comp nohs

totalWinnings :: [(Hand, Int)] -> Int
totalWinnings xs =
    sum $ uncurry (*) <$> zip bids multipliers
  where
    bids = snd <$> sortBy specialCompare xs
    multipliers = [1 .. (length xs)]

bids2 xs = snd <$> sortBy specialCompare xs
multipliers2 xs = [1 .. (length xs)]

specialCompare :: (Hand, Int) -> (Hand, Int) -> Ordering
specialCompare (hand1, _) (hand2, _)
    | type1 == type2 = compare hand1 hand2
    | otherwise = compare type1 type2
  where
    type1 = typeFromHand hand1
    type2 = typeFromHand hand2

-- tested
labelParser :: Parser Label
labelParser = do
    Label <$> satisfy (`elem` validLabels)
  where
    validLabels :: String
    validLabels = "AKQJT98765432"

-- tested
handsWithBidsParser :: Parser [(Hand, Int)]
handsWithBidsParser = do
    handWithBid `sepBy` char '\n'
  where
    handWithBid :: Parser (Hand, Int)
    handWithBid = do
        hand <- many1 labelParser
        space
        bid <- decimal
        return (Hand hand, bid)
