import Data.List (minimumBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import System.Environment (getArgs)

main :: IO ()
main = do 
        args <- getArgs
        inp <- readFile (head args)
        let result = solve inp
        putStrLn $ show result


solve :: String -> Int
solve inp = minimalFuel
        where positions = parse inp
              left = minimum positions
              right = maximum positions
              fuelUsages = map (\p -> (p, fuelUsage positions p)) [left..right]
              (_, minimalFuel) = minimumBy (comparing snd) $ fuelUsages

parse :: String -> [Int]
parse inp = map read $ splitOn "," inp

fuelUsage :: [Int] -> Int -> Int
fuelUsage positions position = sum $ map (triangle . abs . (-) position) positions

triangle :: Int -> Int
triangle n = n * (n + 1) `div` 2
