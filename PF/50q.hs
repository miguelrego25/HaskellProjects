import Data.Char
import Data.Either
import Data.List

--1
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' first last  
       |first > last = first:enumFromTo'(first - 1) last 
       |first == last = [first]
       |first < last = first:enumFromTo'(first + 1) last 

--2
enumFromThenTod :: Int -> Int -> Int -> [Int]
enumFromThenTod a b c 
    |b==0 = []
    |a >= b || b >= c = [a]
    |otherwise = a:enumFromThenTod b ((2*b)-a) c

--3
concat'' :: [a] -> [a] -> [a]
concat'' [] [] = []
concat'' [] l1 = l1
concat'' (a:b) [] = a:b
concat'' (a:b) l1 = a:concat'' b l1
--4
encontra :: [a] -> Int -> a
encontra (h:t) a =
     if a == 0 then h 
     else  encontra t (a-1)

--5
reverse' :: [a] -> [a] 
reverse' [] = []
reverse' l1 = last l1 : reverse' (init l1)

--6
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' a (h:t)
      |a<0=[]
      |otherwise = h:take' (a-1) t

--7
drop' :: Int -> [a] -> [a]
drop' 0 _ = []
drop' _ [] = []
drop' a (h:t)
     |a<0 = (h:t)
     |otherwise = drop' (a-1) t

--8 
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' l1 l2 = (head l1,head l2):zip' (tail l1) (tail l2)

--9
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False 
elem' a (h:t) = if a == h then True else elem' a t

--10
replicate' :: Int -> a -> [a]
replicate' 0 a = []
replicate' b a = a: replicate' (b-1) a

--11
intersperse' :: a -> [a] -> [a]
intersperse' a [] = []
intersperse' a (h:t) = h:a: intersperse' a t

--12
group' :: Eq a => [a] -> [[a]]
group' [] = [] 
group' [a] = [[a]]
group' (h:t) = if h == (head t) then (h:y):ys else [h]:y:ys
      where (y:ys) = group' t

--13
concat' :: [[a]] -> [a] 
concat' [] = []   
concat' (h:t) = h ++ (concat' t)

--14
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' a = inits' (init a) ++ [a]

--15
tails' :: [a] -> [[a]] 
tails' [] =[[]]
tails' a = [a] ++ tails' (tail a)

--16
isPrefixOf' :: Eq a => [a] -> [a] -> Bool 
isPrefixOf' [] [] = True 
isPrefixOf' [] l2 = True
isPrefixOf' l1 l2 
     |(head l1) == (head l2) = isPrefixOf' (tail l1) (tail l2)
     |(head l1) /= (head l2) = False 

--17
isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] [] = True 
isSuffixOf' [] l2 = True 
isSuffixOf' l1 l2 
    |(last l1) == (last l2) = isSuffixOf' (init l1) (init l2) 
    |(last l1) /= (last l2) = False 

--18
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool 
isSubsequenceOf' [] [] = True
isSubsequenceOf' [] l2 = True
isSubsequenceOf' l1 [] = False   
isSubsequenceOf' (x:xs) (y:ys) 
    |x==y = isSubsequenceOf' xs ys 
    |otherwise = isSubsequenceOf' (x:xs) ys

--19
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' a l1 
    |a == (last l1) = elemIndices' a (init l1) ++ [((length l1)-1)]
    |otherwise = elemIndices' a (init l1)

--20
nub' :: Eq a => [a] -> [a] 
nub' [] = []
nub' (h:t) 
    |elem h t = nub' t 
    |otherwise = h:nub' t

--21
delete' :: Eq a => a -> [a] -> [a]
delete' a [] = [] 
delete' a (h:t) 
    |a==h = t 
    |otherwise = h:delete' a t

--22
deletes' :: Eq a => [a] -> [a] -> [a]
deletes' (x:xs) [] = []
deletes' [] (y:ys) = (y:ys)
deletes' l1 l2 
    |elem (head l1) l2 = deletes'(tail l1) (tail l2)
    |otherwise = (head l2):deletes' (tail l1) (tail l2)

--23
union' :: Eq a => [a] -> [a] -> [a]
union' [] (x:xs) = (x:xs)
union' (y:ys) [] = (y:ys)
union' (y:ys) (x:xs) 
    |y == x = y:union' ys xs
    |otherwise = y:union' ys (x:xs) 

--24
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] l1 = l1
intersect' (x:xs) l1
    |elem x l1 = x:intersect' xs l1
    |otherwise = intersect xs l1

--25
insert' :: Ord a => a -> [a] -> [a]
insert' a [] = [a]
insert' a (x:xs) = if a>x then  x:insert' a xs else a:x:xs
    
--26
unwords :: [String] -> String    
unwords [] = ""
unwords [x] = x
 

--32
pospares :: [a]-> [a]
pospares [] = []
pospares [x] = [x]
pospares (h:t:z) = h : pospares z

insereMSet' :: Eq a => a -> [(a,Int)] -> [(a,Int)] 
insereMSet' x [] = []
insereMSet' x ((a,b): y) 
    |x==a = ((a,b+1): y) 
    |otherwise = (a,b) : insereMSet' x y


inits2 :: [a] -> [[a]]
inits2 [] = [[]]
inits2 l@(h:t) = inits2 (initAux l) ++ [l]

initAux :: [a] -> [a]
initAux [x] = []
initAux (h:t) = h : initAux t  

initsA

