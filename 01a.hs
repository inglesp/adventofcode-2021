import System.Environment
import System.IO

main :: IO ()
main = do 
        args <- getArgs
        inh <- openFile (head args) ReadMode
        inpStr <- hGetContents inh
        let result = countIncreases $ readInts inpStr
        putStrLn $ show result

readInts :: String -> [Int]
readInts contents = map read (lines contents)

countIncreases :: [Int] -> Int
countIncreases xs = length $ filter (\(x, y) -> x < y) (zip xs (tail xs))
