













-- on generalise (autant que possible) le type des fonctions du bloc1

--myHead :: [element] -> element
myHead :: [a] -> a
--myHead :: [Int] -> Int
myHead (x:_) = x

myTail :: [a] -> [a]
myTail (_:xs) = xs

myAppend :: [a] -> [a] -> [a]
myAppend xs ys = myAppend' xs 
    where --myAppend' :: [b] -> [b]
          myAppend' (x:xs) = x:myAppend' xs
          myAppend' [] = ys  

myInit :: [a] -> [a]
myInit [_] = []
myInit (x:xs) = x:(myInit xs)

myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

-- O(1)
myNull :: [a] -> Bool
myNull [] = True
myNull _ = False

myNull' :: [a] -> Bool  -- O(n)
myNull' xs = length xs == 0

myLength :: [a] -> Int
myLength (_:xs) = 1 + myLength xs
myLength [] = 0

myReverse :: [a] -> [a]
myReverse (x:xs) = myAppend (myReverse xs) [x]
myReverse xs = xs

myConcat :: [[a]] -> [a]
myConcat (xs:xss) = xs ++ myConcat xss
myConcat [] = []

myTake :: Int -> [a] -> [a]
myTake 0 _  = []
myTake n [] = []
myTake n (x:xs) = x:myTake (n-1) xs

myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop n [] = []
myDrop n (x:xs) = myDrop (n-1) xs

myBangBang :: [a] -> Int -> a
myBangBang (x:xs) 0 = x
myBangBang (x:xs) n = myBangBang xs (n-1)

myInsert :: Ord a => a -> [a] -> [a]
myInsert x [] = [x]
myInsert x (y:ys) | x>y       = y:myInsert x ys
                  | otherwise = x:y:ys

mySort :: Ord b => [b] -> [b]
mySort (x:xs) = myInsert x (mySort xs)
mySort [] = []

myNull'' :: Eq a => [a] -> Bool
myNull'' xs = xs==[]

-- NEW STUFF

-- ordre superieur

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f (x:xs) | f x = x:myTakeWhile f xs
                     | otherwise = []
myTakeWhile f [] = []

-- donner le type de la fonction, notation infixe versus prefixe
myCompose ::  (b -> c) -> (a -> b) -> a -> c
myCompose f g x = f (g x)

myMap :: (a -> b) -> [a] -> [b]
myMap f (x:xs) = f x:myMap f xs
myMap f []     = []

test1 = myMap odd [1..10]

-- calcul des sous liste en utilisant map

sousListes :: [a] -> [[a]]
sousListes (x:xs) = map (x:) (sousListes xs) ++ sousListes xs
sousListes [] = [[]]

-- 1) ne pas tenter de derouler une fonction ecrite

-- 2) ne pas tenter de derouler une fonction pour l'ecrire

-- 3) faire confiance (hypotheses), oublier qui on est et deleguer

-- une fonction plus generale: foldr
-- inferer le type de foldr
-- forme graphique de la liste en peigne
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f k (x:xs) = f x (myFoldr f k xs)
myFoldr f k []     = k

myAnd' :: [Bool] -> Bool
myAnd' bs = foldr (&&) True bs 

-- definir reverse avec foldr
myReverse' :: [a] -> [a]
myReverse' = undefined

-- une parenthese sur les lambda anonymes

add' :: Int -> Int -> Int
add' x y = x + y

add'' :: Int -> Int -> Int
add'' = \x -> \y -> x + y

-- avec foldr
myReverse'' :: [a] -> [a]
myReverse'' = undefined

-- eta reduction
myReverse''' :: [a] -> [a]
myReverse''' = undefined

-- un "nouveau type" String

prenom :: String
prenom = "jean francois"


-- un nouveau type tuples

myFst :: (a,b) -> a
myFst (x,y) = x

-- TODO: definir recursivement

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile = undefined

myElem :: Eq a => a -> [a] -> Bool
myElem = undefined

myNotElem :: Eq a => a -> [a] -> Bool
myNotElem = undefined

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter = undefined

mySplitAt :: Int -> [a] -> ([a],[a])
mySplitAt = undefined

myZip :: [a] -> [b] -> [(a,b)] 
myZip = undefined

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c] 
myZipWith = undefined

myCurry :: ((a,b) -> c) -> a -> b -> c
myCurry = undefined

myUncurry :: (a -> b -> c) -> (a,b) -> c
myUncurry = undefined

myZipWith' :: (a -> b -> c) -> [a] -> [b] -> [c] 
myZipWith' = undefined

myUnzip :: [(a,b)] -> ([a],[b])
myUnzip = undefined

-- TODO: redefinir en utilisant foldr

myConcat' :: [[a]] -> [a]
myConcat' = undefined

myMap' ::  (a -> b) -> [a] -> [b]
myMap' = undefined

myOr' ::  [Bool] -> Bool
myOr' = undefined

myAny :: (a -> Bool) -> [a] -> Bool
myAny = undefined

myAll :: (a -> Bool) -> [a] -> Bool
myAll = undefined

myProduct :: [Int] -> Int
myProduct = undefined

-- TODO: calculuer les 50 plus petits nombres premiers 2, 3, 5, 7, 11...

premiers :: [Int]
premiers = undefined

test2 = take 50 premiers

