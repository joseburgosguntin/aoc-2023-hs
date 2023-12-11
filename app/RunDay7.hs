module Main where

import Day7 (Hand (..), Label (..), totalWinnings)
import qualified Day7 (part1, part2)
import PrintDay (printDay)

main :: IO ()
main = do
    print $ totalWinnings hands
    printDay 7 Day7.part1

-- printDay 7 Day7.part2

hands :: [(Hand, Int)]
hands =
    [ (Hand [Label '3', Label '2', Label 'T', Label '3', Label 'K'], 765)
    , (Hand [Label 'T', Label '5', Label '5', Label 'J', Label '5'], 684)
    , (Hand [Label 'K', Label 'K', Label '6', Label '7', Label '7'], 28)
    , (Hand [Label 'K', Label 'T', Label 'J', Label 'J', Label 'T'], 220)
    , (Hand [Label 'Q', Label 'Q', Label 'Q', Label 'J', Label 'A'], 483)
    ]
