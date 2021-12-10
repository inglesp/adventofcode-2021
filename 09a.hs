import Data.Char (digitToInt)
import System.Environment (getArgs)

main :: IO ()
main = do 
        args <- getArgs
        inp <- readFile (head args)
        let result = solve inp
        putStrLn $ show result

solve :: String -> Int
solve inp = sum $ map (+ 1) $ lowPointHeights $ heightsWithNeighbours $ pad grid
    where grid = parse inp

pad :: [[Int]] -> [[Int]]
pad grid = [paddedRow] ++ (map padRow grid) ++ [paddedRow]
    where
        paddedRow = (take (2 + length (grid !! 0)) $ repeat 9)
        padRow row = [9] ++ row ++ [9]

heightsWithNeighbours :: [[Int]] -> [(Int, [Int])]
heightsWithNeighbours paddedGrid =
    [heightWithNeighbours paddedGrid x y | x <- [1..w], y <- [1..b]]
    where 
        w = (length paddedGrid) - 2
        b = (length (paddedGrid !! 0)) - 2

heightWithNeighbours :: [[Int]] -> Int -> Int -> (Int, [Int])
heightWithNeighbours paddedGrid x y = (h 0 0, [h 1 0, h (-1) 0, h 0 1, h 0 (-1)])
    where h dx dy = paddedGrid !! (x+dx) !! (y+dy)

lowPointHeights :: [(Int, [Int])] -> [Int]
lowPointHeights heightsWithNeighbours = map fst $ filter isLowerThanNeighbours heightsWithNeighbours

isLowerThanNeighbours :: (Int, [Int]) -> Bool
isLowerThanNeighbours (h, hs) = all (\h1 -> h < h1) hs

parse :: String -> [[Int]]
parse inp = map (map digitToInt) $ lines inp
