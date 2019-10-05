import Test.HUnit
import TP1Acompleter

-- test1 = TestCase (assertEqual "for (foo 3)," (True) (True))
-- test [ "test1" ~: "(foo 3)" ~: (1,2) ~=? (foo 3),

-- myInit :: [Int] -> [Int]
tests = test [
        -- "testMyInit1" ~: "for myInit []," ~: (init []) ~=? (myInit []),
        "testMyInit2" ~: "for myInit [1]," ~: (init [1]) ~=? (myInit [1]),
        "testMyInit3" ~: "for myInit [1,2]," ~: (init [1,2]) ~=? (myInit [1,2]),
        "testMyInit4" ~: "for myInit [1,2]," ~: (init [2,1]) ~=? (myInit [2,1]),
    ] 
-- myLast :: [Int] -> Int

-- myNull :: [Int] -> Bool

-- myNull' :: [Int] -> Bool

-- myLength :: [Int] -> Int

-- myReverse :: [Int] -> [Int]

-- myReverse' :: [Int] -> [Int]

-- myAppendIt :: [Int] -> [Int] -> [Int]

-- myConcat :: [[Int]] -> [Int]

-- myAnd :: [Bool] -> Bool

-- myOr ::  [Bool] -> Bool

-- myProduct :: [Int] -> Int

-- myTake :: Int -> [Int] -> [Int]

-- myDrop :: Int -> [Int] -> [Int]

-- myBangBang :: [Int] -> Int -> Int

-- myInsert :: Int -> [Int] -> [Int]

-- mySort :: [Int] -> [Int]
