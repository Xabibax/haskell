{-# LANGUAGE TemplateHaskell #-}
module TestTP2ACompleter where
import Test.QuickCheck
import TP2ACompleter

-- myDropWhile :: (a -> Bool) -> [a] -> [a]
prop_myDropWhile :: Eq a => (a -> Bool) -> [a] -> Bool
prop_myDropWhile f xs = myDropWhile f xs == dropWhile f xs

-- myElem :: Eq a => a -> [a] -> Bool
prop_myElem :: Eq a => a -> [a] -> Bool
prop_myElem x ys = myElem x ys == elem x ys

-- myNotElem :: Eq a => a -> [a] -> Bool
prop_myNotElem :: Eq a => a -> [a] -> Bool
prop_myNotElem x ys = myNotElem x ys == notElem x ys

-- myFilter :: (a -> Bool) -> [a] -> [a]
prop_myFilter :: Eq a => (a -> Bool) -> [a] -> Bool
prop_myFilter f xs = myFilter f xs == filter f xs

-- mySplitAt :: Int -> [a] -> ([a],[a])
prop_mySplitAt :: Eq a => Int -> [a] -> Bool
prop_mySplitAt i xs = mySplitAt i xs == splitAt i xs

-- myZip :: [a] -> [b] -> [(a,b)] 
prop_myZip :: Ord a => [a] -> [a] -> Bool
prop_myZip xs ys = myZip xs ys == zip xs ys

-- myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c] 
prop_myZipWith :: (Eq a, Eq b, Eq c) => (a -> b -> c) -> [a] -> [b] -> Bool
prop_myZipWith f xs ys = myZipWith f xs ys == zipWith f xs ys

-- myCurry :: ((a,b) -> c) -> a -> b -> c
prop_myCurry :: (Eq a, Eq b, Eq c) => ((a,b) -> c) -> a -> b -> Bool
prop_myCurry f x y = myCurry f x y == curry f x y

-- myUncurry :: (a -> b -> c) -> (a,b) -> c
prop_myUnCurry :: (Eq a, Eq b, Eq c) => (a -> b -> c) -> (a,b) -> Bool
prop_myUnCurry f t = myUncurry f t == uncurry f t

-- myZipWith' :: (a -> b -> c) -> [a] -> [b] -> [c] 
prop_myZipWith' :: (Eq a, Eq b, Eq c) => (a -> b -> c) -> [a] -> [b] -> Bool
prop_myZipWith' f xs ys = myZipWith' f xs ys == zipWith f xs ys

-- myUnzip :: [(a,b)] -> ([a],[b])
prop_myUnZip :: (Eq a, Eq b) => [(a,b)] -> Bool
prop_myUnZip ts = myUnzip ts == unzip ts

-- myConcat' :: [[a]] -> [a]
prop_myConcat' :: Eq a => [[a]] -> Bool
prop_myConcat' xss = myConcat' xss == concat xss

-- myMap' ::  (a -> b) -> [a] -> [b]
prop_myMap' :: (Eq a, Eq b) => (a -> b) -> [a] -> Bool
prop_myMap' f xs = myMap' f xs == map f xs

-- myOr' ::  [Bool] -> Bool
prop_myOr' :: [Bool] -> Bool
prop_myOr' xs = myOr' xs == or xs

-- myAny :: (a -> Bool) -> [a] -> Bool
prop_myAny :: Eq a => (a -> Bool) -> [a] -> Bool
prop_myAny f xs = myAny f xs == any f xs

-- myAll :: (a -> Bool) -> [a] -> Bool
prop_myAll :: Eq a => (a -> Bool) -> [a] -> Bool
prop_myAll f xs = myAll f xs == all f xs

-- myProduct :: [Int] -> Int
prop_myProduct :: [Int] -> Bool
prop_myProduct xs = myProduct xs == product xs

-- premiers :: [Int]

return []
runTests = $quickCheckAll