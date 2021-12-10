import Data.List.Split (splitOn)
import System.Environment (getArgs)

main :: IO ()
main = do 
        args <- getArgs
        inp <- readFile (head args)
        let result = solve inp
        putStrLn $ show result

solve :: String -> Int
solve inp = length $ filter (`elem` [2, 3, 4, 7]) $ concat outputLengths
    where
        lines = parse inp
        outputs = map (!! 1) lines
        outputLengths = map (map length) outputs

parse :: String -> [[[String]]]
parse inp = map (map words) $ map (splitOn " | ") $ lines inp
