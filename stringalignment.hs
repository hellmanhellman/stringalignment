module StringAlignment where
import Data.List
import Data.Ord

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1
string1 = "writers"
string2 = "vintner"

type AlignmentType = (String,String)

-- similarityScore :: String -> String -> Int


maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = [a | a <- xs, valueFcn a == valueFcn (maximumBy (comparing valueFcn) xs)]

main = print $ maximaBy length ["cs", "efd", "lth", "it"]
