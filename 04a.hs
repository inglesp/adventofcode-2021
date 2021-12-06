import Data.List.Split (splitOn)
import Data.List (elemIndex, minimumBy, transpose)
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
solve inp = winningBoardScore * lastDrawn
        where (draws, boards) = parse inp
              boardsWithWinningIx = map (\b -> (b, boardWinningIx draws b)) boards
              (winningBoard, winningIx) = minimumBy (comparing (snd)) boardsWithWinningIx
              winningBoardScore = scoreBoard draws winningBoard winningIx
              lastDrawn = draws !! winningIx

parse :: String -> ([Int], [[[Int]]])
parse inp = (draws, boards)
        where rawDraws : rawBoards = splitOn "\n\n" inp
              draws = map read $ splitOn "," rawDraws
              boards = map parseBoard rawBoards
              parseBoard rawBoard = map (map read . words) (lines rawBoard)

boardWinningIx :: [Int] -> [[Int]] -> Int
boardWinningIx draws board = minimum $ map lineWinningIx lines
        where lines = board ++ (transpose board)
              lineWinningIx line = maximum $ map (drawIx draws) line

scoreBoard :: [Int] -> [[Int]] -> Int -> Int
scoreBoard draws board winningIx = sum $ filter (\item -> (drawIx draws item) > winningIx) (concat board)

drawIx draws item = fromJust $ elemIndex item draws
