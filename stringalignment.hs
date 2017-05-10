
-- Q1: Assuming we had access to an algorithm for the string alignment problem, how could we use it to solve MCS for strings?
-- A:  Set scoreMatch = 1 and the others to 0.

-- Q2: Explain what the following Haskell function does.
-- A: It takes a list of tuples of lists and attaches a two prefixes to each tuple in the list.
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

-- Calculates the similarity score between two strings.
-- Implementation is self-documenting.
similarityScore :: String -> String -> Int
similarityScore [] xs = scoreSpace * (length xs)
similarityScore xs [] = scoreSpace * (length xs)

-- Could've used maximum and a list here, but looks better with max3.
similarityScore (x:xs) (y:ys) = max3 ((similarityScore xs ys) + (score x y))
                                     ((similarityScore xs (y:ys)) + (score x '-'))
                                     ((similarityScore (x:xs) ys) + (score '-' y))


-- More efficient implementation, using memoization.
-- This means that no (or at least much fewer) redundant calculations, as
-- reusable data is stored in a lookup table during the computation.
similarityScore' :: String -> String -> Int
similarityScore' xs ys = simScore (length xs) (length ys)
  where
    simScore :: Int -> Int -> Int
    simScore i j = simTable!!i!!j
    simTable = [[ simEntry i j | j<-[0..]] | i<-[0..] ]

    simEntry :: Int -> Int -> Int
    simEntry 0 j = scoreSpace*j -- Following the sides of the table (adding spaces).
    simEntry i 0 = scoreSpace*i -- Following the sides of the table (adding spaces).
    simEntry i j = max3 ((simScore (i-1) (j-1)) + (score x y))
                        ((simScore (i-1)    j ) + (score x '-'))
                        ((simScore     i (j-1)) + (score '-' y))
      where
         x = xs!!(i-1)
         y = ys!!(j-1)

-- Returns the score of two compared characters.
score :: Char -> Char -> Int
score _ '-' = scoreSpace
score '-' _  = scoreSpace
score x y = if x == y
            then scoreMatch
            else scoreMismatch


maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = [a | a <- xs, valueFcn a == valueFcn (maximumBy (comparing valueFcn) xs)]


attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

attachTails :: a -> a -> [([a],[a])] -> [([a],[a])]
attachTails h1 h2 aList = [(xs++[h1],ys++[h2]) | (xs,ys) <- aList]

-- Outputs a list of all the optimum alignments. Memoized.
optAlignments :: String -> String -> [AlignmentType]
optAlignments xs ys = snd (optAlign (length xs) (length ys))
        where
          optAlign :: Int -> Int -> (Int, [AlignmentType])
          optAlign i j = table!!i!!j
          table = [[optEntry i j | j <- [0..]] | i <- [0..]]

          optEntry :: Int -> Int -> (Int, [AlignmentType])
          optEntry 0 j = (scoreSpace*j, [(replicate j '-',take j  ys)])
          optEntry i 0 = (scoreSpace*i, [(take i xs, replicate i '-')])
          optEntry i j  = (simScore, [alignment | alignment <- alignments, alignmentScore alignment == simScore])
--          optEntry i j  = (simScore, maximaBy alignmentScore alignments) -- this is slow
            where

              ls1 | score == bestAdjacentScore = attachTails x y (snd (optAlign (i-1) (j-1)))
                  | otherwise                  = []
                 where score = fst (optAlign (i-1) (j-1))

              ls2 | score == bestAdjacentScore = attachTails '-'  y  (snd (optAlign i (j-1)))
                  | otherwise                  = []
                 where score = fst (optAlign i (j-1))

              ls3 | score == bestAdjacentScore = attachTails x  '-'  (snd (optAlign (i-1) j))
                  | otherwise                  = []
                 where score = fst (optAlign (i-1) j)

              bestAdjacentScore = max3 (fst (optAlign (i-1) (j-1))) (fst (optAlign i (j-1))) (fst (optAlign (i-1) j))
              alignments =  ls1 ++ ls2 ++ ls3
              simScore = similarityScore' (take i xs) (take j ys)
              x = xs!!(i-1)
              y = ys!!(j-1)

-- Calculates the score of an alignment.
-- Almost the same as similarityScore, but this function calculates the score of an alignment,
-- whereas similarityScore calculates the best possible score of two strings.
alignmentScore :: AlignmentType -> Int
alignmentScore ([], xs) = scoreSpace * (length xs)
alignmentScore (xs, []) = scoreSpace * (length xs)
alignmentScore ((x:xs), (y:ys)) = alignmentScore (xs,ys) + score x y




-- Pretty prints the alignments.
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
                                  spacey (x:[]) = [x]
                                  spacey (x:xs) = [x,' '] ++ spacey xs

long1 = "aferociousmonadatemyhamster"
long2 = "functionalprogrammingrules"
main = outputOptAlignments long1 long2
