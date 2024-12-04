main = do
    input <- getContents
    let reports = map words $ lines input
    print $ countSafe reports

countSafe :: [[String]] -> Int
countSafe (x:xs) = foldr ((+) . findSafe . map read) 0 xs

findSafe :: [Int] -> Int
findSafe xs = if safeIncreasing xs || safeDecreasing xs then 1 else 0

safeIncreasing :: [Int] -> Bool
safeIncreasing [] = True
safeIncreasing [x] = True
safeIncreasing (x:y:xs) = 1 <= diff && diff <= 3 && safeIncreasing (y:xs) where
    diff = y - x

safeDecreasing :: [Int] -> Bool
safeDecreasing [] = True
safeDecreasing [x] = True
safeDecreasing (x:y:xs) = 1 <= diff && diff <= 3 && safeDecreasing (y:xs) where
    diff = x - y
