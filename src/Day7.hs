{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char (digitToInt, isDigit)
import Data.List (partition, sort, sortBy, sortOn)
import Data.Text (Text, group, pack)
import Text.Read (Lexeme (String))

part1 :: Parser Int
part1 = totalWinnings <$> handsWithBidsParser

part2 :: Parser Int
part2 = undefined

newtype Card = Card Char
    deriving (Eq, Show)

instance Ord Card where
    Card a <= Card b = case (a, b) of
        (_, 'A') -> True
        (a, 'K') | a /= 'A' -> True
        (a, 'Q') | a /= 'A' && a /= 'K' -> True
        (a, 'J') | a /= 'A' && a /= 'K' && a /= 'Q' -> True
        (a, 'T') | a /= 'A' && a /= 'K' && a /= 'Q' && a /= 'T' -> True
        (a, b) | isDigit a && isDigit b -> digitToInt a <= digitToInt b
        _ -> False

-- is a 5 element array
newtype Hand = Hand [Card]
    deriving (Eq, Show)

instance Ord Hand where
    Hand cs1 <= Hand cs2
        | t1 == t2 = cs1 <= cs2
        | otherwise = t1 <= t2
      where
        t1 = typeFromHand $ Hand cs1
        t2 = typeFromHand $ Hand cs2

data Type
    = HighCard
    | OnePair
    | TwoPair
    | ThreeOfAKind
    | FullHouse
    | FourOfAKind
    | FiveOfAKind -- strongest
    deriving (Show, Eq, Ord)

labelToChar (Card char) = char

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
    bids = snd <$> sort xs
    multipliers = [1 .. (length xs)]

labelParser :: Parser Card
labelParser = do
    Card <$> satisfy (`elem` validLabels)
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
