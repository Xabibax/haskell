import Test.QuickCheck
import TP2ACompleter
import Data.List

-- myDropWhile :: (a -> Bool) -> [a] -> [a]

-- myElem :: Eq a => a -> [a] -> Bool

-- myNotElem :: Eq a => a -> [a] -> Bool

-- myFilter :: (a -> Bool) -> [a] -> [a]

-- mySplitAt :: Int -> [a] -> ([a],[a])

-- myZip :: [a] -> [b] -> [(a,b)] 
test_myZip :: Ord a => [a] -> [a] -> Bool
test_myZip xs ys = myZip xs ys == zip xs ys

-- myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c] 
-- test_myZipWith :: Ord a => (a -> a -> a) -> [a] -> [a] -> Bool
-- test_myZipWith f xs ys = test_myZipWith f xs ys == zipWith f xs ys

-- myCurry :: ((a,b) -> c) -> a -> b -> c

-- myUncurry :: (a -> b -> c) -> (a,b) -> c

-- myZipWith' :: (a -> b -> c) -> [a] -> [b] -> [c] 

-- myUnzip :: [(a,b)] -> ([a],[b])

-- myConcat' :: [[a]] -> [a]

-- myMap' ::  (a -> b) -> [a] -> [b]

-- myOr' ::  [Bool] -> Bool

-- myAny :: (a -> Bool) -> [a] -> Bool

-- myAll :: (a -> Bool) -> [a] -> Bool

-- myProduct :: [Int] -> Int

-- premiers :: [Int]
