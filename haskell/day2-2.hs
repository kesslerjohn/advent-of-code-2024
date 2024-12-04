main = do
    input <- getContents
    let reports = map words $ lines input
    print $ countSafe reports

-- >>> findSafe [7, 6, 4, 2, 1]
-- 1

-- >>> findSafe [1, 2, 7, 8, 9]
-- 0

-- >>> findSafe [9, 7, 6, 2, 1]
-- 0

-- >>> findSafe [1, 3, 2, 4, 5]
-- 1

-- >>> findSafe [8, 6, 4, 4, 1]
-- 1

-- >>> findSafe [1, 3, 6, 7, 9]
-- 1

-- >>> strictDecreasing [1, 3, 2, 4, 5]
-- False

-- >>> findSafe []
-- 0

countSafe :: [[String]] -> Int
countSafe = sum . preProcess

preProcess :: [[String]] -> [Int]
preProcess = map (findSafe . map read)

findSafe :: [Int] -> Int
findSafe xs = if not (null xs) && (anyIncreasing xs || anyDecreasing xs) then 1 else 0

anyIncreasing :: [Int] -> Bool
anyIncreasing xs = strictIncreasing xs || skipIncreasing 1 xs

anyDecreasing :: [Int] -> Bool
anyDecreasing xs = strictDecreasing xs || skipDecreasing 1 xs

-- The first argument holds the original array
-- the second argument holds what needs to be checked
strictIncreasing :: [Int] -> Bool
strictIncreasing (x:y:xs) = (1 <= diff && diff <= 3) && strictIncreasing (y:xs) where
    diff = y - x
strictIncreasing _        = True

strictDecreasing :: [Int] -> Bool
strictDecreasing (x:y:xs) = (1 <= diff && diff <= 3) && strictDecreasing (y:xs) where
    diff = x - y
strictDecreasing _        = True

-- true if the sequence is safe by skipping the xth element
-- or by skipping any one element later
skipIncreasing :: Int -> [Int] -> Bool
skipIncreasing x ns 
    | length ns < x = False
    | otherwise     = strictIncreasing (skip x ns) || skipIncreasing (x+1) ns

-- true if the sequence is safe by skipping the xth element
-- or by skipping any one element later
skipDecreasing :: Int -> [Int] -> Bool
skipDecreasing x ns 
    | length ns < x = False
    | otherwise     = strictDecreasing (skip x ns) || skipDecreasing (x+1) ns

-- drops the nth element of a list of integers
-- one-indexed
skip :: Int -> [Int] -> [Int]
skip x ns
    | length ns < x = ns
    | otherwise     = take (x-1) ns ++ drop x ns

