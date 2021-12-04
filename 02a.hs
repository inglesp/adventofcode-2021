import System.Environment
import System.IO
import Text.Regex.Posix

main :: IO ()
main = do 
        args <- getArgs
        inh <- openFile (head args) ReadMode
        inpStr <- hGetContents inh
        let result = process inpStr
        putStrLn $ show result

process :: String -> Int
process inp = pos * depth
        where (pos, depth) = computeCoords $ parseLines inp

computeCoords :: [(String, Int)] -> (Int, Int)
computeCoords = foldl step (0, 0)

step :: (Int, Int) -> (String, Int) -> (Int, Int)
step (pos, depth) ("forward", s) = (pos + s, depth)
step (pos, depth) ("up", s) = (pos, depth - s)
step (pos, depth) ("down", s) = (pos, depth + s)

parseLines :: String -> [(String, Int)]
parseLines inp = map parseLine $ lines inp

parseLine :: String -> (String, Int)
parseLine line = (dir, read s)
        where [[_, dir, s]] = line =~ "([a-z]+) ([0-9]+)$" :: [[String]]
