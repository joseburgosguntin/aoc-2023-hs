{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text (parseOnly)
import qualified Day7
import qualified System.Exit as Exit
import Test.HUnit

part1 = TestCase (assertEqual "" (Right 6440) $ parseSample Day7.part1)

-- part2 = TestCase (assertEqual "" (Right 71503) $ parseSample Day7.part2)
tests =
    TestList
        [ TestLabel "part1" part1
        -- , TestLabel "part2" part2
        ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

parseSample parser =
    parseOnly
        parser
        "\
        \32T3K 765\n\
        \T55J5 684\n\
        \KK677 28\n\
        \KTJJT 220\n\
        \QQQJA 483\n\
        \"
