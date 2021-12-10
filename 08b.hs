import Data.List (sort)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import System.Environment (getArgs)

main :: IO ()
main = do 
        args <- getArgs
        inp <- readFile (head args)
        let lines = parse inp
        let result = solve inp
        putStrLn $ show result

solve :: String -> Int
solve inp = sum $ map solveLine lines
    where
        lines = parse inp

solveLine :: [[String]] -> Int
solveLine line = toInt outputDigits
    where
        ii:oo:_ = line
        digitMap = findDigits ii
        outputDigits = map ((Map.!) digitMap) oo


findDigits ii = Map.fromList [
    (find0 ii, 0),
    (find1 ii, 1),
    (find2 ii, 2),
    (find3 ii, 3),
    (find4 ii, 4),
    (find5 ii, 5),
    (find6 ii, 6),
    (find7 ii, 7),
    (find8 ii, 8),
    (find9 ii, 9)]

find0 ii = head $ filter (\i -> (length i == 6) && (i /= find6 ii) && (i /= find9 ii)) ii
find1 ii = head $ filter (\i -> length i == 2) ii
find2 ii = head $ filter (\i -> (length i == 5) && (i /= find5 ii) && (i /= find3 ii)) ii
find3 ii = head $ filter (\i -> (length i == 5) && (all (`elem` i) (find1 ii))) ii
find4 ii = head $ filter (\i -> length i == 4) ii
find5 ii = head $ filter (\i -> (length i == 5) && (all (`elem` (find6 ii)) i)) ii
find6 ii = head $ filter (\i -> (length i == 6) && (not $ all (`elem` i) (find1 ii))) ii
find7 ii = head $ filter (\i -> length i == 3) ii
find8 ii = head $ filter (\i -> length i == 7) ii
find9 ii = head $ filter (\i -> (length i == 6) && (all (`elem` i) (find3 ii))) ii

toInt :: (Foldable t, Num b) => t b -> b
toInt = fst . foldr (\b (acc, p) -> (acc + p * b, 10 * p)) (0, 1)

parse :: String -> [[[String]]]
parse = map parseLine . lines

parseLine :: String -> [[String]]
parseLine line = map ((map sort) . words) (splitOn " | " line)
