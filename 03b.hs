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
process inp = (toInt $ ogr lines) * (toInt $ csr lines)
        where lines = parseLines inp

ogr :: [[Int]] -> [Int]
ogr lines = getValue lines mostCommon 0

csr :: [[Int]] -> [Int]
csr lines = getValue lines leastCommon 0

getValue :: [[Int]] -> ([Int] -> Int) -> Int -> [Int]
getValue lines fn ix =
        let lines' = filter (\line -> line !! ix == fn ((transpose lines) !! ix)) lines
        in  if (length lines' == 1)
            then lines' !! 0
            else getValue lines' fn (ix + 1)

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
