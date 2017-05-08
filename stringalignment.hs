
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

score :: Char -> Char -> Int
score _ '-' = scoreSpace
score '-' _  = scoreSpace
score x y = if x == y
            then scoreMatch
            else scoreMismatch

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = [a | a <- xs, valueFcn a == valueFcn (maximumBy (comparing valueFcn) xs)]





main = print $ similarityScore' string1 string2


attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

attachTails :: a -> a -> [([a],[a])] -> [([a],[a])]
attachTails h1 h2 aList = [(xs++[h1],ys++[h2]) | (xs,ys) <- aList]

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

alignmentScore :: (String, String) -> Int
alignmentScore ([], xs) = scoreSpace * (length xs)
alignmentScore (xs, []) = scoreSpace * (length xs)
alignmentScore ((x:xs), (y:ys)) = alignmentScore (xs,ys) + score x y

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

