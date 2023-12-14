{-# LANGUAGE OverloadedStrings #-}

module ParseDay (parseDay, printDay) where

import Data.Attoparsec.Text (Parser, parseOnly)
import Data.Text (Text, pack)
import System.Directory (getCurrentDirectory)

parseDay :: String -> Parser a -> IO a
parseDay day parser = do
    dir <- getCurrentDirectory
    let dayPath = dir <> "/input/Day" <> day <> ".txt"
    txt <- fromFile dayPath
    case parseOnly parser txt of
        Right res -> return res
        Left e -> fail e
  where
    fromFile :: FilePath -> IO Text
    fromFile path = pack <$> readFile path

printDay :: (Show a) => String -> Parser a -> IO ()
printDay day parser = parseDay day parser >>= print
