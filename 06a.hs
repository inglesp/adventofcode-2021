import Data.List (group, sort)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

main :: IO ()
main = do 
        args <- getArgs
        inp <- readFile (head args)
        let result = solve inp
        putStrLn $ show result

solve :: String -> Int
solve inp = sum final
        where initialFrequencies = frequencies $ parse inp
              initial = map (\i -> fromMaybe 0 (Map.lookup i initialFrequencies)) [0..8]
              final = (repeatedlyApply 80 step) initial

parse :: String -> [Int]
parse inp = map read $ splitOn "," inp

frequencies :: Ord a => [a] -> Map.Map a Int
frequencies = Map.fromList . map (\x -> (head x, length x)) . group . sort

step :: [Int] -> [Int]
step [x0, x1, x2, x3, x4, x5, x6, x7, x8] = [x1, x2, x3, x4, x5, x6, x0 + x7, x8, x0]

repeatedlyApply :: Int -> (a -> a) -> (a -> a)
repeatedlyApply n f = foldr (.) id (replicate n f)
