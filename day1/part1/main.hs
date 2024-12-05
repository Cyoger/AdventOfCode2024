import System.IO (readFile)
import Data.List (sort)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let (list1, list2) = parseColumns contents
    
    let sortedList1 = sort list1
    let sortedList2 = sort list2
    
    let totalDistance = sum $ zipWith (\x y -> abs (x - y)) sortedList1 sortedList2
    print totalDistance

parseColumns :: String -> ([Int], [Int])
parseColumns contents =
    let rows = lines contents
        pairs = map parseLine rows
    in unzip pairs

parseLine :: String -> (Int, Int)
parseLine line = 
    let [x, y] = map read (words line)
    in (x, y)
