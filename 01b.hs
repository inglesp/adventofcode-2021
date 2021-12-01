import System.Environment
import System.IO

main :: IO ()
main = do 
        args <- getArgs
        inh <- openFile (head args) ReadMode
        inpStr <- hGetContents inh
        let result = countIncreases $ buildWindowSums $ readInts inpStr
        putStrLn $ show result

readInts :: String -> [Int]
readInts contents = map read (lines contents)

buildWindowSums :: [Int] -> [Int]
buildWindowSums xs = map (\(x, y, z) -> x + y + z) (zip3 xs (tail xs) (tail $ tail xs))

countIncreases :: [Int] -> Int
countIncreases xs = length $ filter (\(x, y) -> x < y) (zip xs (tail xs))
