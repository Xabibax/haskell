import Test.QuickCheck
import TP2ACompleter
import Data.List

-- myDropWhile :: (a -> Bool) -> [a] -> [a]
test_myDropWhile :: Eq a => (a -> Bool) -> [a] -> Bool
test_myDropWhile f xs = myDropWhile f xs == dropWhile f xs

-- myElem :: Eq a => a -> [a] -> Bool
test_myElem :: Eq a => a -> [a] -> Bool
test_myElem x ys = myElem x ys == elem x ys

-- myNotElem :: Eq a => a -> [a] -> Bool
test_myNotElem :: Eq a => a -> [a] -> Bool
test_myNotElem x ys = myNotElem x ys == notElem x ys

-- myFilter :: (a -> Bool) -> [a] -> [a]
test_myFilter :: Eq a => (a -> Bool) -> [a] -> Bool
test_myFilter f xs = myFilter f xs == filter f xs

-- mySplitAt :: Int -> [a] -> ([a],[a])
test_mySplitAt :: Eq a => Int -> [a] -> Bool
test_mySplitAt i xs = mySplitAt i xs == splitAt i xs

-- myZip :: [a] -> [b] -> [(a,b)] 
test_myZip :: Ord a => [a] -> [a] -> Bool
test_myZip xs ys = myZip xs ys == zip xs ys

-- myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c] 
test_myZipWith :: (Eq a, Eq b, Eq c) => (a -> b -> c) -> [a] -> [b] -> Bool
test_myZipWith f xs ys = myZipWith f xs ys == zipWith f xs ys

-- myCurry :: ((a,b) -> c) -> a -> b -> c
test_myCurry :: (Eq a, Eq b, Eq c) => ((a,b) -> c) -> a -> b -> Bool
test_myCurry f x y = myCurry f x y == curry f x y

-- myUncurry :: (a -> b -> c) -> (a,b) -> c
test_myUnCurry :: (Eq a, Eq b, Eq c) => (a -> b -> c) -> (a,b) -> Bool
test_myUnCurry f t = myUncurry f t == uncurry f t

-- myZipWith' :: (a -> b -> c) -> [a] -> [b] -> [c] 
test_myZipWith' :: (Eq a, Eq b, Eq c) => (a -> b -> c) -> [a] -> [b] -> Bool
test_myZipWith' f xs ys = myZipWith' f xs ys == zipWith f xs ys

-- myUnzip :: [(a,b)] -> ([a],[b])
test_myUnZip :: (Eq a, Eq b) => [(a,b)] -> Bool
test_myUnZip ts = myUnzip ts == unzip ts

-- myConcat' :: [[a]] -> [a]
test_myConcat' :: Eq a => [[a]] -> Bool
test_myConcat' xss = myConcat' xss == concat xss

-- myMap' ::  (a -> b) -> [a] -> [b]
test_myMap' :: (Eq a, Eq b) => (a -> b) -> [a] -> Bool
test_myMap' f xs = myMap' f xs == map f xs

-- myOr' ::  [Bool] -> Bool
test_myOr' :: [Bool] -> Bool
test_myOr' xs = myOr' xs == or xs

-- myAny :: (a -> Bool) -> [a] -> Bool
test_myAny :: Eq a => (a -> Bool) -> [a] -> Bool
test_myAny f xs = myAny f xs == any f xs

-- myAll :: (a -> Bool) -> [a] -> Bool
test_myAll :: Eq a => (a -> Bool) -> [a] -> Bool
test_myAll f xs = myAll f xs == all f xs

-- myProduct :: [Int] -> Int
test_myProduct :: [Int] -> Bool
test_myProduct xs = myProduct xs == product xs

-- premiers :: [Int]
