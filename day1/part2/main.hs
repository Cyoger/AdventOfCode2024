import System.IO (readFile)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let (list1, list2) = parseColumns contents
    let counts = map (\x -> (x, countOccurrences x list1)) list2
    print(result counts)

result :: [(Int, Int)] -> Int
result counts = sum (map (uncurry (*)) counts) 

countOccurrences :: Int -> [Int] -> Int
countOccurrences n list = length (filter (== n) list)

parseColumns :: String -> ([Int], [Int])
parseColumns contents =
    let rows = lines contents
        pairs = map parseLine rows
    in unzip pairs

parseLine :: String -> (Int, Int)
parseLine line = 
    let [x, y] = map read (words line)
    in (x, y)