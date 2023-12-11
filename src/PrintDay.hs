{-# LANGUAGE OverloadedStrings #-}

module PrintDay (printDay) where

import Data.Attoparsec.Text
import Data.Text (Text, pack)
import System.Directory

-- TODO: add condtion where it checks args
-- if the day is equal then show something
-- only make this change if want only Main.hs
printDay :: (Show a, Show b) => a -> Parser b -> IO ()
printDay day parser = do
    currentDir <- getCurrentDirectory
    let inputDir = currentDir <> "/input"
    let dayPath = inputDir <> "/Day" <> show day <> ".txt"
    print $ "Day " <> show day
    txt <- fromFile dayPath
    print $ parseOnly parser txt
  where
    fromFile :: FilePath -> IO Text
    fromFile path = pack <$> readFile path
