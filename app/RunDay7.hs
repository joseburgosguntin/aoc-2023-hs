module Main where

import qualified Day7 (part1, part2)
import ParseDay (printDay)

main :: IO ()
main = do
    let printDay7 = printDay "7"
    printDay7 Day7.part1
