import Data.List
import System.Environment
import System.IO

main :: IO ()
main = do 
        args <- getArgs
        inh <- openFile (head args) ReadMode
        inp <- hGetContents inh
        let result = process inp
        putStrLn $ show result

process :: String -> Int
process inp = (gamma cols) * (epsilon cols)
        where cols = transpose $ parseLines inp

gamma :: [[Int]] -> Int
gamma cols = toInt (map mostCommon cols)

epsilon :: [[Int]] -> Int
epsilon cols = toInt (map leastCommon cols)

-- toInt [1, 0, 1, 0] = 0 * 1 + 1 * 2 + 0 * 4 + 1 * 8 = 10
toInt :: [Int] -> Int
toInt = fst . foldr (\b (acc, p) -> (acc + p * b, 2 * p)) (0, 1)

mostCommon :: [Int] -> Int
mostCommon xx
        | 2 * (sum xx) < (length xx) = 0
        | otherwise                  = 1

leastCommon :: [Int] -> Int
leastCommon xx
        | 2 * (sum xx) < (length xx) = 1
        | otherwise                  = 0

parseLines :: String -> [[Int]]
parseLines inp = map parseLine $ lines inp

parseLine :: String -> [Int]
parseLine = map (read . (:""))
