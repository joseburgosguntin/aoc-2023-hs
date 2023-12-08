module Main where

import qualified Day6 (part1, part2)
import PrintDay (printDay)

main :: IO ()
main = do
    printDay 6 Day6.part1
    printDay 6 Day6.part2
