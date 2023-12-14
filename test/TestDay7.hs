{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text (parseOnly)
import Data.Char (intToDigit)
import Data.List (sort, sortOn)
import Day7
import ParseDay
import qualified System.Exit as Exit
import Test.HUnit

cardOrd :: Test
cardOrd = TestCase (sort unsorted @?= unsorted)
  where
    nums = intToDigit <$> [2 .. 9]
    unsorted = Card <$> nums <> "TJQKA"

hands :: [(Hand, Int)]
hands =
    toHands
        [ ("32T3K", 765)
        , ("KTJJT", 220)
        , ("KK677", 28)
        , ("T55J5", 684)
        , ("QQQJA", 483)
        ]
  where
    toHands pairs = do
        (str, bid) <- pairs
        return (Hand $ Card <$> str, bid)

handOrd :: Test
handOrd = TestCase (sortOn fst hands @?= hands)

typeFromHandTest :: Test
typeFromHandTest = TestCase (fromHands @?= types)
  where
    types = [OnePair, TwoPair, TwoPair, ThreeOfAKind, ThreeOfAKind]
    fromHands = typeFromHand . fst <$> hands

totalWinningsTest :: Test
totalWinningsTest = TestCase (totalWinnings hands @?= 6440)

-- handsWithBidsParserTest :: Test
handsWithBidsParserTest =
    TestCase
        (parseDay "7Test" handsWithBidsParser >>= (@?= hands) . sort)

part1Solution :: Test
part1Solution =
    TestCase (parseDay "7Test" part1 >>= assertEqual "" 6440)

part1Tests :: Test
part1Tests =
    TestLabel "part1" $
        TestList
            [ TestLabel "cardOrd" cardOrd
            , TestLabel "handOrd" handOrd
            , TestLabel "typeFromHand" typeFromHandTest
            , TestLabel "totalWinnings" totalWinningsTest
            , TestLabel "handsWithBidsParser" handsWithBidsParserTest
            , TestLabel "solution" part1Solution
            ]

part2Tests :: Test
part2Tests =
    TestLabel "part2" $
        TestList
            []

tests :: Test
tests = TestList [part1Tests, part2Tests]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0
        then Exit.exitFailure
        else Exit.exitSuccess
