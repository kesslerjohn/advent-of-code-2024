import Data.List

main = do
    input <- getContents
    let start = map words $ lines input
    let mid = map sort $ transpose start
    let fin = doWork $ intTuplify mid
    print fin

intTuplify :: [[String]] -> ([Int], [Int])
intTuplify xs = (map read as, map read bs) where
    [as, bs] = take 2 xs

doWork :: ([Int], [Int]) -> Int
doWork (a:as, b:bs) = (a * length (filter (==a) (b:bs))) + doWork (as, b:bs)
doWork (_, _) = 0