
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





main = outputOptAlignments string1 string2


attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]


optAlignments :: String -> String -> [AlignmentType]
optAlignments xs ys = [tuple | tuple <- allCombinations xs ys, tupleScore tuple == bestScore]
  where bestScore = similarityScore xs ys

allCombinations :: String -> String -> [AlignmentType]
allCombinations [] xs = [(replicate (length xs) '-', xs)]
allCombinations xs [] = [(xs, (replicate (length xs) '-'))]
allCombinations (x:xs) (y:ys) = attachHeads x y (allCombinations xs ys)
                             ++ attachHeads x '-' (allCombinations xs (y:ys))
                             ++ attachHeads '-' y (allCombinations (x:xs) ys)

tupleScore :: (String, String) -> Int
tupleScore tuple = simpleScore (fst tuple) (snd tuple)

simpleScore :: String -> String -> Int
simpleScore [] xs = scoreSpace * (length xs)
simpleScore xs [] = scoreSpace * (length xs)
simpleScore (x:xs) (y:ys) = (simpleScore xs ys) + (score x y)

spacey :: String -> String
spacey (x:[]) = [x]
spacey (x:xs) = [x,' '] ++ spacey xs


outputOptAlignments :: String -> String -> IO ()
outputOptAlignments xs ys = do  putStrLn $ "There are " ++ show (length result) ++ " optimal alignments:"
                                putStrLn ""
                                printAlignments result
                                putStrLn ""
                                putStrLn $ "There were " ++ show (length result) ++ " optimal alignments!"
                            where result = optAlignments xs ys
                                  printAlignments :: [AlignmentType] -> IO ()
                                  printAlignments (x:xs) = do putStrLn $ spacey (fst x)
                                                              putStrLn $ spacey (snd x)
                                                              putStrLn ""
                                                              if xs /= [] then printAlignments xs
                                                                else return ()

