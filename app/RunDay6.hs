module Main where

import qualified Day6 (part1, part2)
import ParseDay (printDay)

main :: IO ()
main = do
    let printDay6 = printDay "6"
    printDay6 Day6.part1
    printDay6 Day6.part2
