import Test.QuickCheck
import TP2ACompleter
import Data.List

test_myZip :: [Int] -> [Int] -> Bool
test_myZip xs ys = myZip xs ys == zip xs ys

-- test_myZipWith :: (Int -> Int -> Int) -> [Int] -> [Int] -> Bool
-- test_myZipWith f xs ys = test_myZipWith f xs ys == zipWith f xs ys