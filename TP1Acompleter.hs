module TP1Acompleter where
    
import Data.List







-- :r


-- commentaires, multilignes

un :: Int
un = 1

deux :: Int
deux = un + un

mySub :: Int -> Int -> Int
mySub x y = x-y

neg :: Int -> Int
--neg x = mySub 0 x
neg = mySub 0

-- constante entiere, identifiant, declaration typee, definition

l0 :: [Int]
l0 = []

l1 :: [Int]
l1 = 1:l0

l2 :: [Int]
l2 = 2:l1
--l2 = 2 mal type

l3 :: [Int]
l3 = [1,2,3]

l4 :: [Int]
l4 = [1..10]

l5 :: [Int]
l5 = [1,3..10]

myHead :: [Int] -> Int
myHead (x:xs) = x

l6 :: [Int]
l6 = [10,8..0]

myTail :: [Int] -> [Int]
myTail (x:xs) = xs

-- fonction, declaration typee, definition


-- application partielle


-- booleen


-- fonction recursive

myAppend :: [Int] -> [Int] -> [Int]
myAppend (x:xs) ys = x:myAppend xs ys
myAppend []     ys = ys
--myAppend [x]    ys = x:ys

myAppend' :: [Int] -> [Int] -> [Int]
myAppend' xs ys | not (null xs) = head xs : myAppend' (tail xs) ys
                | otherwise     = ys

myNull12 :: [Int] -> Bool
myNull12 (x:xs) = False
myNull12 xs     = True
--myNull12 []     = True



-- liste d'entier, nil, cons, liste en comprehension


-- pattern matching


  







myAppend'' :: [Int] -> [Int] -> [Int]
myAppend'' xs ys | null xs       = ys
                 | not (null xs) = head xs : myAppend'' (tail xs) ys

myAppend4 :: [Int] -> [Int] -> [Int]
myAppend4 (x:xs) ys = 
    let suite = myAppend4 xs ys 
    in x:suite
myAppend4 []     ys = ys

myAppend5 :: [Int] -> [Int] -> [Int]
myAppend5 (x:xs) ys = x:suite where suite = myAppend5 xs ys
myAppend5 []     ys = ys

myAppend6 :: [Int] -> [Int] -> [Int]
myAppend6 xs ys = myAppend6' xs 
    where myAppend6' (x:xs) = x:myAppend6' xs
          myAppend6' []     = ys

-- a vous...
myInit :: [Int] -> [Int]
myInit [] = init [] -- to return the same error as init 
myInit [x] = []
myInit (x:xs) = x:myInit xs

myLast :: [Int] -> Int
myLast [x] = x
myLast (x:xs) = myLast (xs)

myNull :: [Int] -> Bool
myNull (x:xs)   = False
myNull []       = True

myNull' :: [Int] -> Bool
myNull' []      = True
myNull' (x:xs)  = False

myLength :: [Int] -> Int
myLength (x:xs) = 1 + myLength xs
myLength []     = 0

myReverse :: [Int] -> [Int]
myReverse xs | (myNull xs) = []
             | otherwise   = myLast xs:myReverse (myInit xs )

-- iteratif, comparer les complexites experimentalement
myAppendIt :: [Int] -> [Int] -> [Int]
myAppendIt xs ys = myAppendIt' (myReverse' xs) ys
    where myAppendIt' xs [] = myReverse' xs
          myAppendIt' xs (y:ys) = myAppendIt' (y:xs) ys

myLength' :: [Int] -> Int
myLength' xs = myLength'' xs
    where myLength'' (x:xs) = 1 + myLength'' xs
          myLength'' []     = 0

myReverse' :: [Int] -> [Int]
myReverse' xs = myReverse'' xs []
    where myReverse'' (x:xs) ys = myReverse'' xs (x:ys)
          myReverse'' [] ys = ys

myConcat :: [[Int]] -> [Int]
myConcat ys = myConcat' [] ys
    where myConcat' xs (y:ys) = myConcat' (myAppendIt xs y) ys
          myConcat' xs [] = xs

myAnd :: [Bool] -> Bool
myAnd xs = myAnd' xs True
    where myAnd' (x:xs) y = myAnd' xs (x && y)
          myAnd' [] y  = y

myOr ::  [Bool] -> Bool
myOr xs = myOr' xs False
    where myOr' (x:xs) y = myOr' xs (x || y)
          myOr' [] y  = y

myProduct :: [Int] -> Int
myProduct xs = myProduct' xs 1
    where myProduct' (x:xs) y = myProduct' xs (x * y)
          myProduct' [] y  = y

-- pas d'element neutre pour max et min 

myTake :: Int -> [Int] -> [Int]
myTake x ys = myTake' x ys
    where myTake' 0 ys = ys
          myTake' x ys = myTake' (x-1) (myInit ys)

myDrop :: Int -> [Int] -> [Int]
myDrop x ys = myDrop' x ys
    where myDrop' 0 ys = ys
          myDrop' x (y:ys) = myDrop' (x-1) (ys)

-- cette fonction existe sous le nom !!
myBangBang :: [Int] -> Int -> Int
myBangBang xs y = myBangBang' xs y
    where myBangBang' (x:xs) 0 = x
          myBangBang' (x:xs) y = myBangBang' xs (y-1)

myInsert :: Int -> [Int] -> [Int]
myInsert x ys = myInsert' x ys []
    where myInsert' x [] zs = myReverse' (x:zs)
          myInsert' x (y:ys) zs| x<=y = myAppendIt (myReverse' zs) (x:y:ys)
                               | otherwise = myInsert' x ys (y:zs) 

mySort :: [Int] -> [Int]
mySort xs 
    = mySort' xs []
    where mySort' [] ys = ys
          mySort' (x:xs) ys = mySort' xs (myInsert x ys)
