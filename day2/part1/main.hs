import System.IO (readFile)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let line = lines contents
    let validCount = foldl processLine 0 line
    print validCount

processLine ::  Int -> String -> Int
processLine count line = do
    let numbers = map read (words line) :: [Int]
    if isSortedAndValidDiff numbers
        then count + 1
        else count

isSortedAndValidDiff :: [Int] -> Bool
isSortedAndValidDiff xs = isSorted xs && hasDiff xs

isAscending :: [Int] -> Bool
isAscending [] = True
isAscending [_] = True
isAscending (x:y:xs) = x <= y && isAscending (y:xs)


isDescending :: [Int] -> Bool
isDescending [] = True
isDescending [_] = True
isDescending (x:y:xs) = x >= y && isDescending (y:xs)

isSorted :: [Int] -> Bool
isSorted xs = isAscending xs || isDescending xs

hasDiff :: [Int] -> Bool
hasDiff [] = True
hasDiff [_] = True
hasDiff (x:y:xs) = abs (x - y) >= 1 && abs (x - y) <= 3 && hasDiff (y:xs)

