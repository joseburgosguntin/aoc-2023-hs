{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text (parseOnly)
import qualified Day6
import qualified System.Exit as Exit
import Test.HUnit

part1 = TestCase (assertEqual "" (Right 288) $ parseSample Day6.part1)
part2 = TestCase (assertEqual "" (Right 71503) $ parseSample Day6.part2)
tests = TestList [TestLabel "part1" part1, TestLabel "part2" part2]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

parseSample parser =
    parseOnly
        parser
        "\
        \Time:      7  15   30\n\
        \Distance:  9  40  200\n\
        \"
