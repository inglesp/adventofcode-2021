import Data.List (sort, transpose)
import Data.Maybe (catMaybes)
import Data.Set (Set, fromList, toList, union)
import System.Environment (getArgs)

main :: IO ()
main = do 
        args <- getArgs
        inp <- readFile (head args)
        let result = solve inp
        putStrLn $ show result

solve :: String -> Int
solve inp = product $ take 3 $ reverse $ sort $ map length $ toList $ fromList $ catMaybes $ concat $ run $ parse inp

parse :: String -> [[Maybe (Set (Int, Int))]]
parse inp = [[initial x y | y <- [0..b-1]] |  x <- [0..w-1]]
    where
        grid = lines inp
        w = length grid
        b = length (grid !! 0)
        initial x y = parseOne x y (grid !! x !! y)

parseOne :: Int -> Int -> Char -> Maybe (Set (Int, Int))
parseOne _ _ '9' = Nothing
parseOne x y _ = Just (fromList [(x, y)])

run ::  [[Maybe (Set (Int, Int))]] -> [[Maybe (Set (Int, Int))]]
run grid = converge (transpose . step . transpose . step) grid

step :: [[Maybe (Set (Int, Int))]] -> [[Maybe (Set (Int, Int))]]
step grid = map (converge stepLine) grid

stepLine :: [Maybe (Set (Int, Int))] -> [Maybe (Set (Int, Int))]
stepLine line = map stepOne [1..(length line)]
    where
        l = [Nothing] ++ line ++ [Nothing]
        stepOne ix = merge (l !! ix) (l !! (ix - 1)) (l !! (ix + 1))

merge :: Ord a => Maybe (Set a) -> Maybe (Set a) -> Maybe (Set a) -> Maybe (Set a)
merge (Just s) (Just lhs) (Just rhs) = Just(lhs `union` s `union` rhs)
merge (Just s) (Just lhs) Nothing = Just(lhs `union` s)
merge (Just s) Nothing (Just rhs) = Just(s `union` rhs)
merge (Just s) Nothing Nothing = Just s
merge Nothing _ _ = Nothing

converge :: Eq t => (t -> t) -> t -> t
converge f input =
    let output = f input
    in if output == input
       then input
       else converge f output
