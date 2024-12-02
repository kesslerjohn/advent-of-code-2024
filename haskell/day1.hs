import Data.List
main = do
    input <- getContents
    let start = map words $ lines input
    let res = doWork $ intTuplify $ map sort $ transpose start 
    print res

intTuplify :: [[String]] -> ([Int], [Int])
intTuplify xs = (map read as, map read bs) where
    [as, bs] = take 2 xs

doWork :: ([Int], [Int]) -> Int
doWork (a:as, b:bs) = abs (a - b) + doWork (as, bs)
doWork (_, _) = 0