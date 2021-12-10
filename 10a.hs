import Data.Either (rights)
import System.Environment (getArgs)

main :: IO ()
main = do 
    args <- getArgs
    inp <- readFile (head args)
    let result = solve inp
    putStrLn $ show result

solve :: String -> Int
solve inp = sum $ map score $ rights parses
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

score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137
