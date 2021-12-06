import Data.List (group, sort)
import System.Environment (getArgs)
import Text.Regex.Posix

main :: IO ()
main = do 
        args <- getArgs
        inp <- readFile (head args)
        let result = solve inp
        putStrLn $ show result

solve :: String -> Int
solve inp = length $ map fst $ filter (\(_, n) -> n > 1) $ frequencies allCoords
        where entries = map parseLine (lines inp)
              allCoords = concat $ map coords entries

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine line = ((read x1, read y1), (read x2, read y2))
        where [[_, x1, y1, x2, y2]] = line =~ "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)" :: [[String]]

coords :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
coords ((x1, y1), (x2, y2)) | (x1 == x2) = zip (repeat x1) (range y1 y2)
                            | (y1 == y2) = zip (range x1 x2) (repeat y1)
                            | otherwise  = zip (range x1 x2) (range y1 y2)

range :: Int -> Int -> [Int]
range m n | m < n = [m..n]
          | otherwise = [m,(m-1)..n]

frequencies :: Ord a => [a] -> [(a, Int)]
frequencies = map (\x -> (head x, length x)) . group . sort
