#!/usr/bin/env runghc

import System.IO (readFile)
import Text.Regex.TDFA ((=~))
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let matches = extractMul contents
    let result = sum $ map calculateProduct matches
    print result

extractMul :: String -> [(Int, Int)]
extractMul input =
    let pattern = "mul\\(([0-9]+),([0-9]+)\\)"
        matches = input =~ pattern :: [[String]]
    in mapMaybe parseMatch matches

parseMatch :: [String] -> Maybe (Int, Int)
parseMatch [_, x, y] = Just (read x, read y)
parseMatch _ = Nothing

calculateProduct :: (Int, Int) -> Int
calculateProduct (x, y) = x * y
