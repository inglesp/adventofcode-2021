import Data.List.Split (splitOn)
import Data.List (elemIndex, maximumBy, transpose)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import System.Environment
import System.IO

main :: IO ()
main = do 
        args <- getArgs
        inp <- readFile (head args)
        let result = solve inp
        putStrLn $ show result

solve :: String -> Int
solve inp = losingBoardScore * lastDrawn
        where (draws, boards) = parse inp
              boardsWithLosingIx = map (\b -> (b, boardLosingIx draws b)) boards
              (losingBoard, losingIx) = maximumBy (comparing (snd)) boardsWithLosingIx
              losingBoardScore = scoreBoard draws losingBoard losingIx
              lastDrawn = draws !! losingIx

parse :: String -> ([Int], [[[Int]]])
parse inp = (draws, boards)
        where rawDraws : rawBoards = splitOn "\n\n" inp
              draws = map read $ splitOn "," rawDraws
              boards = map parseBoard rawBoards
              parseBoard rawBoard = map (map read . words) (lines rawBoard)

boardLosingIx :: [Int] -> [[Int]] -> Int
boardLosingIx draws board = minimum $ map lineLosingIx lines
        where lines = board ++ (transpose board)
              lineLosingIx line = maximum $ map (drawIx draws) line

scoreBoard :: [Int] -> [[Int]] -> Int -> Int
scoreBoard draws board losingIx = sum $ filter (\item -> (drawIx draws item) > losingIx) (concat board)

drawIx draws item = fromJust $ elemIndex item draws
