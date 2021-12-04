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
        where (pos, depth, _) = computeCoords $ parseLines inp

computeCoords :: [(String, Int)] -> (Int, Int, Int)
computeCoords = foldl step(0, 0, 0)

step :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
step (pos, depth, aim) ("forward", s) = (pos + s, depth + (aim * s), aim)
step (pos, depth, aim) ("up", s) = (pos, depth, aim - s)
step (pos, depth, aim) ("down", s) = (pos, depth, aim + s)

parseLines :: String -> [(String, Int)]
parseLines inp = map parseLine $ lines inp

parseLine :: String -> (String, Int)
parseLine line = (dir, read s)
        where [[_, dir, s]] = line =~ "([a-z]+) ([0-9]+)$" :: [[String]]
