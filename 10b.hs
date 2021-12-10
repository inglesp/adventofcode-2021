import Data.Either (lefts)
import Data.List (sort)
import System.Environment (getArgs)

main :: IO ()
main = do 
    args <- getArgs
    inp <- readFile (head args)
    let result = solve inp
    putStrLn $ show result

solve :: String -> Int
solve inp = median $ map score $ lefts parses
    where
        parses = map parseLine $ lines inp

parseLine :: String -> Either [Char] Char
parseLine s = foldl f (Left []) s

f :: Either [Char] Char -> Char -> Either [Char] Char
f (Left stack) '(' = Left ('(' : stack)
f (Left stack) '[' = Left ('[' : stack)
f (Left stack) '{' = Left ('{' : stack)
f (Left stack) '<' = Left ('<' : stack)
f (Left ('(' : rest)) ')' = Left (rest)
f (Left ('[' : rest)) ']' = Left (rest)
f (Left ('{' : rest)) '}' = Left (rest)
f (Left ('<' : rest)) '>' = Left (rest)
f (Left _) c= Right c
f (Right c) _ = Right c


score :: [Char] -> Int
score stack = score' 0 stack

score' :: Int -> [Char] -> Int
score' acc []      = acc
score' acc (c:stack) = score' (5 * acc + (s c)) stack
    where
        s '(' = 1
        s '[' = 2
        s '{' = 3
        s '<' = 4

median :: [Int] -> Int
median xs = (sort xs) !! ix
    where ix = ((length xs) - 1) `div` 2
