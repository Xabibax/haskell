import Test.HUnit
import TP1Acompleter
import Data.List

-- test1 = TestCase (assertEqual "for (foo 3)," (True) (True))
-- test [ "test1" ~: "(foo 3)" ~: (1,2) ~=? (foo 3),

tests = test [
-- myInit :: [Int] -> [Int]
    -- "testMyInit1" ~: "for myInit []," ~: (init []) ~=? (myInit []),
    "testMyInit2" ~: "for myInit [1],"      ~: (init [1])       ~=? (myInit [1]),
    "testMyInit3" ~: "for myInit [1,2],"    ~: (init [1,2])     ~=? (myInit [1,2]),
    "testMyInit3" ~: "for myInit [1,2,3],"  ~: (init [1,2,3])   ~=? (myInit [1,2,3]),
    "testMyInit4" ~: "for myInit [3,1,2],"  ~: (init [3,1,2])   ~=? (myInit [3,1,2]),

-- myLast :: [Int] -> Int
    -- "testMyLast1" ~: "for myLast [1]," ~: (last []) ~=? (myLast []),
    "testMyLast2" ~: "for myLast [1],"      ~: (last [1])       ~=? (myLast [1]),
    "testMyLast3" ~: "for myLast [1,2],"    ~: (last [1,2])     ~=? (myLast [1,2]),
    "testMyLast3" ~: "for myLast [1,2,3],"  ~: (last [1,2,3])   ~=? (myLast [1,2,3]),
    "testMyLast4" ~: "for myLast [3,1,2],"  ~: (last [3,1,2])   ~=? (myLast [3,1,2]),

-- myNull :: [Int] -> Bool
    "testMyNull1" ~: "for myNull [],"       ~: (null [])        ~=? (myNull []),
    "testMyNull2" ~: "for myNull [0],"      ~: (null [0])       ~=? (myNull [0]),
    "testMyNull3" ~: "for myNull [1],"      ~: (null [1])       ~=? (myNull [1]),
    "testMyNull4" ~: "for myNull [2,1],"    ~: (null [2,1])     ~=? (myNull [2,1]),

-- myNull' :: [Int] -> Bool
    "testMyNull'1" ~: "for myNull' [],"     ~: (null [])        ~=? (myNull' []),
    "testMyNull'2" ~: "for myNull' [0],"    ~: (null [0])       ~=? (myNull' [0]),
    "testMyNull'3" ~: "for myNull' [1],"    ~: (null [1])       ~=? (myNull' [1]),
    "testMyNull'4" ~: "for myNull' [2,1],"  ~: (null [2,1])     ~=? (myNull' [2,1]),

-- myLength :: [Int] -> Int
    "testMyLength1" ~: "for myLength [],"           ~: (length [])              ~=? (myLength []),
    "testMyLength2" ~: "for myLength [0],"          ~: (length [0])             ~=? (myLength [0]),
    "testMyLength3" ~: "for myLength [1],"          ~: (length [1])             ~=? (myLength [1]),
    "testMyLength4" ~: "for myLength [1,2],"        ~: (length [1,2])           ~=? (myLength [1,2]),
    "testMyLength5" ~: "for myLength [2,1,5,8,2],"  ~: (length [2,1,5,8,2])     ~=? (myLength [2,1,5,8,2]),

-- myReverse :: [Int] -> [Int]
    "testMyReverse1" ~: "for myReverse [],"             ~: (reverse [])             ~=? (myReverse []),
    "testMyReverse2" ~: "for myReverse [1],"            ~: (reverse [1])            ~=? (myReverse [1]),
    "testMyReverse3" ~: "for myReverse [1,2],"          ~: (reverse [1,2])          ~=? (myReverse [1,2]),
    "testMyReverse5" ~: "for myReverse [2,1,5,8,2],"    ~: (reverse [2,1,5,8,2])    ~=? (myReverse [2,1,5,8,2]),

-- myReverse' :: [Int] -> [Int]
    "testMyReverse'1" ~: "for myReverse' [],"           ~: (reverse [])             ~=? (myReverse' []),
    "testMyReverse'2" ~: "for myReverse' [1],"          ~: (reverse [1])            ~=? (myReverse' [1]),
    "testMyReverse'3" ~: "for myReverse' [1,2],"        ~: (reverse [1,2])          ~=? (myReverse' [1,2]),
    "testMyReverse'5" ~: "for myReverse' [2,1,5,8,2],"  ~: (reverse [2,1,5,8,2])    ~=? (myReverse' [2,1,5,8,2]),

-- myAppendIt :: [Int] -> [Int] -> [Int]
    "testMyAppendIt1" ~: "for myAppendIt [] [],"        ~: (mappend [] [])          ~=? (myAppendIt [] []) ,
    "testMyAppendIt2" ~: "for myAppendIt [1] [],"       ~: (mappend [1] [])         ~=? (myAppendIt [1] []),
    "testMyAppendIt3" ~: "for myAppendIt [1,2] [],"     ~: (mappend [1,2] [])       ~=? (myAppendIt [1,2] []),
    "testMyAppendIt4" ~: "for myAppendIt [] [1],"       ~: (mappend [] [1])         ~=? (myAppendIt [] [1]),
    "testMyAppendIt5" ~: "for myAppendIt [] [1,2],"     ~: (mappend [] [1,2])       ~=? (myAppendIt [] [1,2]),
    "testMyAppendIt6" ~: "for myAppendIt [1,2] [3,4],"  ~: (mappend [1,2] [3,4])    ~=? (myAppendIt [1,2] [3,4]),
    "testMyAppendIt7" ~: "for myAppendIt [2,1] [4,3],"  ~: (mappend [2,1] [4,3])    ~=? (myAppendIt [2,1] [4,3]),
    "testMyAppendIt8" ~: "for myAppendIt [1,1] [5,5],"  ~: (mappend [1,1] [5,5])    ~=? (myAppendIt [1,1] [5,5]),

-- myConcat :: [[Int]] -> [Int]
    "testMyConcat1" ~: "for myConcat [],"                   ~: (concat [])                  ~=? (myConcat []),
    "testMyConcat2" ~: "for myConcat [[]],"                 ~: (concat [[]])                ~=? (myConcat [[]]),
    "testMyConcat3" ~: "for myConcat [[],[]],"              ~: (concat [[],[]])             ~=? (myConcat [[],[]]),
    "testMyConcat4" ~: "for myConcat [[1],[]],"             ~: (concat [[1],[]])            ~=? (myConcat [[1],[]]),
    "testMyConcat5" ~: "for myConcat [[],[1]],"             ~: (concat [[],[1]])            ~=? (myConcat [[],[1]]),
    "testMyConcat6" ~: "for myConcat [[2],[1]],"            ~: (concat [[2],[1]])           ~=? (myConcat [[2],[1]]),
    "testMyConcat7" ~: "for myConcat [[1,2],[3,4]],"        ~: (concat [[1,2],[3,4]])       ~=? (myConcat [[1,2],[3,4]]),
    "testMyConcat8" ~: "for myConcat [[2,1],[4,3]],"        ~: (concat [[2,1],[4,3]])       ~=? (myConcat [[2,1],[4,3]]),
    "testMyConcat9" ~: "for myConcat [[1,2],[],[3,4]],"     ~: (concat [[1,2],[],[3,4]])    ~=? (myConcat [[1,2],[],[3,4]]),

-- myAnd :: [Bool] -> Bool
    "testMyAnd1" ~: "for myAnd [],"             ~: (and [])             ~=? (myAnd []),
    "testMyAnd2" ~: "for myAnd [True],"         ~: (and [True])         ~=? (myAnd [True]),
    "testMyAnd3" ~: "for myAnd [False],"        ~: (and [False])        ~=? (myAnd [False]),
    "testMyAnd4" ~: "for myAnd [True,True],"    ~: (and [True,True])    ~=? (myAnd [True,False]),
    "testMyAnd5" ~: "for myAnd [True,False],"   ~: (and [True,False])   ~=? (myAnd [True,False]),
    "testMyAnd6" ~: "for myAnd [False,True],"   ~: (and [False,True])   ~=? (myAnd [False,True]),
    "testMyAnd7" ~: "for myAnd [False,False],"  ~: (and [False,False])  ~=? (myAnd [False,False]),

-- myOr ::  [Bool] -> Bool
    "testMyOr1" ~: "for myOr [],"               ~: (or [])              ~=? (myOr []),
    "testMyOr2" ~: "for myOr [True],"           ~: (or [True])          ~=? (myOr [True]),
    "testMyOr3" ~: "for myOr [False],"          ~: (or [False])         ~=? (myOr [False]),
    "testMyOr4" ~: "for myOr [True,True],"      ~: (or [True,True])     ~=? (myOr [True,False]),
    "testMyOr5" ~: "for myOr [True,False],"     ~: (or [True,False])    ~=? (myOr [True,False]),
    "testMyOr6" ~: "for myOr [False,True],"     ~: (or [False,True])    ~=? (myOr [False,True]),
    "testMyOr7" ~: "for myOr [False,False],"    ~: (or [False,False])   ~=? (myOr [False,False]),

-- myProduct :: [Int] -> Int
    "testMyProduct1" ~: "for myProduct [],"     ~: (product [])     ~=? (myProduct []),
    "testMyProduct2" ~: "for myProduct [1],"    ~: (product [1])    ~=? (myProduct [1]),
    "testMyProduct3" ~: "for myProduct [1,2],"  ~: (product [1,2])  ~=? (myProduct [1,2]),
    "testMyProduct4" ~: "for myProduct [2,1],"  ~: (product [2,1])  ~=? (myProduct [2,1]),

-- myTake :: Int -> [Int] -> [Int]
    "testMyTake1" ~: "for myTake [],"           ~: (take 1 [])          ~=? (myTake 1 []),
    "testMyTake2" ~: "for myTake [1],"          ~: (take 1 [1])         ~=? (myTake 1 [1]),
    "testMyTake3" ~: "for myTake [1,2],"        ~: (take 1 [1,2])       ~=? (myTake 1 [1,2]),
    "testMyTake4" ~: "for myTake [2,1],"        ~: (take 1 [2,1])       ~=? (myTake 1 [2,1]),
    "testMyTake5" ~: "for myTake 4 [1,4,5],"    ~: (take 4 [1,4,5])     ~=? (myTake 4 [1,4,5]),
    "testMyTake6" ~: "for myTake 2 [1,4,5],"    ~: (take 2 [1,4,5])     ~=? (myTake 2 [1,4,5]),
    "testMyTake7" ~: "for myTake 0 [1,4,5],"    ~: (take 0 [1,4,5])     ~=? (myTake 0 [1,4,5]),

-- myDrop :: Int -> [Int] -> [Int]
    "testMyDrop1" ~: "for myDrop [],"           ~: (drop 1 [])          ~=? (myDrop 1 []),
    "testMyDrop2" ~: "for myDrop [1],"          ~: (drop 1 [1])         ~=? (myDrop 1 [1]),
    "testMyDrop3" ~: "for myDrop [1,2],"        ~: (drop 1 [1,2])       ~=? (myDrop 1 [1,2]),
    "testMyDrop4" ~: "for myDrop [2,1],"        ~: (drop 1 [2,1])       ~=? (myDrop 1 [2,1]),
    "testMyDrop5" ~: "for myDrop 4 [1,4,5],"    ~: (drop 4 [1,4,5])     ~=? (myDrop 4 [1,4,5]),
    "testMyDrop6" ~: "for myDrop 2 [1,4,5],"    ~: (drop 2 [1,4,5])     ~=? (myDrop 2 [1,4,5]),
    "testMyDrop7" ~: "for myDrop 0 [1,4,5],"    ~: (drop 0 [1,4,5])     ~=? (myDrop 0 [1,4,5]),

-- myBangBang :: [Int] -> Int -> Int
    --"testMyBangBang1" ~: "for myBangBang [] 1," ~: ([]!!1) ~=? (myBangBang [] 1),
    "testMyBangBang2" ~: "for myBangBang [1] 0,"    ~: ([1]!!0)     ~=? (myBangBang [1] 0),
    "testMyBangBang3" ~: "for myBangBang [1,2] 1,"  ~: ([1,2]!!1)   ~=? (myBangBang [1,2] 1),
    "testMyBangBang4" ~: "for myBangBang [2,1] 0,"  ~: ([2,1]!!0)   ~=? (myBangBang [2,1] 0),

-- myInsert :: Int -> [Int] -> [Int]
    "testMyInsert1" ~: "for myInsert [],"           ~: ((insert 1 []))          ~=? (myInsert 1 []),
    "testMyInsert2" ~: "for myInsert [1],"          ~: (insert 1 [1])           ~=? (myInsert 1 [1]),
    "testMyInsert3" ~: "for myInsert [1,2],"        ~: (insert 1 [1,2])         ~=? (myInsert 1 [1,2]),
    "testMyInsert4" ~: "for myInsert [2,1],"        ~: (insert 1 [2,1])         ~=? (myInsert 1 [2,1]),
    "testMyInsert5" ~: "for myInsert 4 [1,4,5],"    ~: (insert 4 [1,4,5])       ~=? (myInsert 4 [1,4,5]),
    "testMyInsert6" ~: "for myInsert 2 [1,4,5],"    ~: (insert 2 [1,4,5])       ~=? (myInsert 2 [1,4,5]),
    "testMyInsert7" ~: "for myInsert 0 [1,4,5],"    ~: (insert 0 [1,4,5])       ~=? (myInsert 0 [1,4,5]),
    "testMyInsert8" ~: "for myInsert 4 [6,1,4,5],"  ~: (insert 4 [6,1,4,5])     ~=? (myInsert 4 [6,1,4,5]),
    "testMyInsert9" ~: "for myInsert 2 [6,1,4,5],"  ~: (insert 2 [6,1,4,5])     ~=? (myInsert 2 [6,1,4,5]),

-- mySort :: [Int] -> [Int]
    "testMySort1" ~: "for mySort [],"               ~: (sort [])                ~=? (mySort []),
    "testMySort2" ~: "for mySort [1],"              ~: (sort [1])               ~=? (mySort [1]),
    "testMySort3" ~: "for mySort [1,2,3,4,5],"      ~: (sort [1,2,3,4,5])       ~=? (mySort [1,2,3,4,5]),
    "testMySort4" ~: "for mySort [2,1],"            ~: (sort [2,1])             ~=? (mySort [2,1]),
    "testMySort4" ~: "for mySort [1,5,3,3,5],"      ~: (sort [1,5,3,3,5])       ~=? (mySort [1,5,3,3,5]),
    "testMySort4" ~: "for mySort [2,1,11,10,01],"   ~: (sort [2,1,11,10,01])    ~=? (mySort [2,1,11,10,01]),

-- End of tests

    "testEnd" ~: "End,"   ~: (True)    ~=? (True)]