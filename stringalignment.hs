
-- Q1: Assuming we had access to an algorithm for the string alignment problem, how could we use it to solve MCS for strings?
-- A:  Set scoreMatch = 1 and the others to 0.

-- Q2: Explain what the following Haskell function does.
-- A: It takes a list of tuples of lists and attaches a prefix to each tuple-list.
--    eg. attachHeads "un" "super" [("cool","sweet")] -> [("uncool","supersweet")]


module StringAlignment where
import Data.List
import Data.Ord

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1
string1 = "writers"
string2 = "vintner"

type AlignmentType = (String,String)

max3 a b c = max a (max b c)

similarityScore :: String -> String -> Int
similarityScore [] xs = scoreSpace * (length xs)
similarityScore xs [] = scoreSpace * (length xs)


similarityScore (x:xs) (y:ys) = max3 ((similarityScore xs ys) + (score x y))
                                     ((similarityScore xs (y:ys)) + (score x '-'))
                                     ((similarityScore (x:xs) ys) + (score '-' y))

score :: Char -> Char -> Int
score _ '-' = scoreSpace
score '-' _  = scoreSpace
score x y = if x == y
            then scoreMatch
            else scoreMismatch

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = [a | a <- xs, valueFcn a == valueFcn (maximumBy (comparing valueFcn) xs)]



main = print $ similarityScore string1 string2
-- main = print $ maximaBy length ["cs", "efd", "lth", "it"]
