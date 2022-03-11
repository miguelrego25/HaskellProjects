--1
enumFromTod :: Int -> Int -> [Int]
enumFromTod x y 
   | x < y = x : enumFromTod (x+1) y
   | x == y = x : enumFromTod (x+1) y
   | otherwise = []


--2
enumFromThenTod ::  Int -> Int -> Int -> [Int]
enumFromThenTod x y z
   | x < z = x: enumFromThenTod y ((2*y) - x) z
   |otherwise = [] 

--3
concatenad :: [a] -> [a] -> [a]
concatenad [] l2 = l2
concatenad l1 [] = l1 
concatenad (x:xs) (y:ys) = x:concatenad xs (y:ys) 

--4
getme :: [a] -> Int -> a
getme (x:xs) a 
   | a>0 = getme xs (a-1)
   | a==0 = x

--5
reversed :: [a] -> [a]
reversed [] = []
reversed (x:xs) = (reversed xs) ++ [x]

--6
taked :: Int -> [a] -> [a]
taked _ [] = []
taked a (x:xs) 
   | a>0 =  x: taked (a-1) xs 

--7 
dropd :: Int -> [a] -> [a] 
dropd _ [] = [] 
dropd 0 l1 = l1
dropd a (x:xs) 
   | a>0 = dropd (a-1) xs

--8
zipd :: [a] -> [b] -> [(a,b)]
zipd [] _ = [] 
zipd _ [] = [] 
zipd (x:xs) (y:ys) = (x,y): zipd xs ys

--9
replicated :: Int -> a -> [a]
replicated 0 a = [] 
replicated n x 
   |n>0 = x:replicated (n-1) x   

--10
interspersed :: a -> [a] -> [a]
interspersed n [x] = [x]
interspersed n (x:xs) = x:n:interspersed n xs 

--11
{-groupd :: Eq a => [a] -> [[a]]
groupd [] = []
groupd [[x]] = [[x]]
groupd (x:xs:xss) 
   | x == xs = inseregrupo (x:xs:xss) 
   | x /= xs = x:groupd(xs:xss)

inseregrupo :: Eq a => [a] ->[[a]]
inseregrupo [] = []
inseregrupo [x] = [[x]]
inseregrupo (x:xs:xss) 
   |x == xs = [x] ++ [xs] ++ inseregrupo (xs:xss)
   |otherwise = groupd(xs:xss)


groupd :: Eq a => [a] -> [[a]]
groupd [] = []
groupd [x] = [[x]]
groupd (x:xs:xss) 
   |x == xs = inseregrupo(x:xs:xss)
   |x /= xs = x:groupd(xs:xss)

inseregrupo :: Eq a => [a] -> [[a]] -> [[a]]
inseregrupo (x:xs:xss:xsss)
   |xs == xss = [x] ++ [xs] ++  r
   |otherwise = [x] ++ [xs] :  r
   where r = groupd (xss:xsss)

 
groupd :: Eq a => [a] -> [[a]] 
groupd (x:xs:xss)
   |elem x (xs:xss) = [x]++[xs] : r
   |otherwise = x : r
   where r = groupd (xs:xss)


-}
--11
group :: Eq a => [a] -> [[a]]
group [] = []
group [x] = [[x]]
group (h:t)
    | elem h (head r) = (h : (head r)) : tail r
    --verifica se o primeiro elemento esta na lista
    | otherwise = [h] : r
    where r = group t


--Resolução alternativa
groupdd :: Eq a => [a] -> [[a]]
groupdd [] = []
groupdd (h:t) = insere h (groupdd t)

insere :: Eq a => a -> [[a]] -> [[a]]
insere x [] = [[x]]
insere x (h:t)
    | elem x h = (x : h) : t
    | otherwise = [x] : (h : t)
-- VER ISTOOOO ---------- 96969696 
--420420420420420420420420420420420420420420420420420420

--12
concatd :: [[a]] -> [a]
concatd [[]] = []
concatd [] = []
concatd [[x]] = [x] 
concatd ((x:xs):ys) = (x:xs) ++ concatd ys  

--Minha Resoluçao Ver porque e que nao esta certo
{-
initsd :: [a] -> [[a]]
initsd ls = ini 0 ls

ini :: Int -> [a] -> [a]
ini a ls
   |a < (length ls) = (take a ls ) ++  ini (a+1) (ls) 
   |a == (length ls) = (take a ls)

-}
{-
take n ls = 
take 0 ls = take 
take 1 ls
take 2 ls 
take 3 ls 
take 4 ls 
-}
--13
initsd :: [a]->[[a]]
initsd [] = [[]]
initsd ls = initsd (init ls) ++ [ls]

--14
tailsd :: [a] -> [[a]]
tailsd [] = [[]]
tailsd ls = [ls] ++ tailsd (tail ls)

--15
headsd :: [[a]] -> [a]
headsd [] = []
headsd (x:xs) = (head x) : headsd (xs)

--16
totald :: [[a]] -> Int 
totald [] = 0
totald (x:xs) = (length x) + totald xs

--17
fund :: [(a,b,c)] -> [(a,c)]
fund [] = [] 
fund ((a,b,c):xs) = (a,c) : fund (xs)

--18
colad :: [([Char],b,c)] -> [Char]
colad [] = [] 
colad (((y:ys),b,c):xs) = (y:ys) ++ colad xs 

--19
idaded :: Int -> Int -> [([Char],Int)] -> [[Char]]
idaded a b [] = []
idaded a b (((x:xs),idade):ys)
   |a - idade >= b = (x:xs) : idaded a b (ys)
   |otherwise = idaded a b (ys)

--20
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n m 
   | m > 0 =  powerEnumFrom n (m-1) ++ [n ^ (m-1)]
   | otherwise = []

--21
isPrime :: Int -> Bool
isPrime n
   | n > 2 = primeCheck 2 n 
   |otherwise = False 

primeCheck :: Int -> Int -> Bool
primeCheck m n
   | m*m > n = True   
   | mod n m == 0 = False 
   | otherwise = primeCheck (m+1) n

--22
isPrefixOfd :: Eq a => [a]-> [a] -> Bool
isPrefixOfd [] _ = True 
isPrefixOfd _ [] = True 
isPrefixOfd (x:xs) (y:ys) 
   | x == y = isPrefixOfd xs ys 
   | otherwise = False

--23 
isSuffixOfd :: Eq a => [a]-> [a] -> Bool
isSuffixOfd (x:xs) [] = False
isSuffixOfd [] (y:ys) = False
isSuffixOfd [] [] = True 
isSuffixOfd (x:xs) (y:ys)
   | x == y && length xs == length ys = isSuffixOfd xs ys
   | otherwise = isSuffixOfd (x:xs) ys

--24
isSubsequenceOfd :: Eq a => [a] -> [a] -> Bool
isSubsequenceOfd [] [] = True
isSubsequenceOfd _ [] = False
isSubsequenceOfd [] _ = True
isSubsequenceOfd (x:xs) (y:ys)
   | x == y = isSubsequenceOfd xs ys 
   | otherwise = isSubsequenceOfd (x:xs) ys

--25
elemIndicesd :: Eq a => a -> [a] -> [Int]
elemIndicesd a (x:xs) = parelem a (x:xs) 1

parelem :: Eq a => a -> [a] -> Int -> [Int]
parelem num [] a = []
parelem num (x:xs) a
   | num == x = a: parelem num xs (a+1)
   | otherwise = parelem num xs (a+1)
--26
nubs :: Eq a => [a] -> [a] 
nubs [] = [] 
nubs (x:xs) = x:nubs(rmvrep((x:xs)))

rmvrep :: Eq a => [a] -> [a] 
rmvrep [x] = []
rmvrep (x:xs:xss) 
   |x==xs = rmvrep(x:xss)
   |otherwise = xs:rmvrep (x:xss)

--27

deletee :: Eq a => a -> [a] -> [a]
deletee _ [] = []
deletee x (h:t)
   | x == h = t
   | otherwise = h : deletee x t

--28
removelista :: Eq a => [a] -> [a]-> [a]
removelista l1 [] = l1
removelista [] l2 = []
removelista (x:xs) (y:ys)
   | x == y = removelista xs ys 
   | otherwise = x:removelista xs (y:ys)

--29
uniond :: Eq a => [a] -> [a] -> [a]
uniond [] [] = []
uniond [] l2 = l2
uniond l1 [] = l1
uniond l (h:t)
    | elem h  l = uniond l t
    | otherwise = uniond (l ++ [h]) t 

--30
intersectd :: Eq a => [a] -> [a] -> [a]
intersectd [] _ = [] 
intersectd l [] = []
intersectd (x:xs) (y:ys)
   | elem x (y:ys) = x:intersectd xs (y:ys) 
   | otherwise = intersectd xs ys

--31
insertd :: Ord a => a -> [a] -> [a]
insertd x [] = [x]
insertd x (h:t)
    | x > h = h : insertd x t
    | otherwise = x : h : t

--32
unwordsd :: [String] -> String
unwordsd [] = []
unwordsd (x:xs) =  x ++ (if null xs then "" else " ") ++ unwordsd xs 

--33
unlinesd :: [String] -> String
unlinesd [] = []
unlinesd (x:xs) =  x ++ "\n" ++ unlinesd xs

--34
--pMaiord :: Ord a => [a] -> Int 

--36 
preCrescente :: Ord a => [a] -> [a] 
preCrescente [] = []
preCrescente (x:xs:xss)
   |x < xs = x: preCrescente(xs:xss)
   |x >= xs = [x]

--37

iSort :: Ord a => [a] -> [a] 
iSort [] = []
iSort [x] = [x]
iSort (x:xs:xss)
   | x > xs = iSort(insertd x (xs:xss))
   | otherwise = x:iSort (xs:xss)

--38
menor :: String -> String -> Bool
menor [] l = True 
menor l [] = False 
menor (x:xs) (y:ys)
   |x < y = True 
   |x > y = False
   |x == y = menor xs ys

--39
elemMSet  :: Eq a => a -> [(a,Int)] -> Bool
elemMSet a [] = False
elemMSet a ((x,y):xs)
   | a == x = True  
   | otherwise = elemMSet a xs

--40
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = [] 
converteMSet ((x,y):xs)
   | y > 0 = x : converteMSet ((x,y-1):xs)
   | y == 0 = converteMSet xs

--41
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] 
insereMSet a [] = [] 
insereMSet a ((x,y):xs)
   | a == x = (x,y+1): insereMSet a xs 
   | otherwise = (x,y): insereMSet a xs 

--42
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] 
removeMSet a [] = [] 
removeMSet a ((x,y):xs)
   | a == x && y == 1 = removeMSet a xs
   | a == x = (x,y-1): removeMSet a xs 
   | otherwise = (x,y): removeMSet a xs 

--43 ver este 
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet (x:xs:xss)
   | x == xs = (x,)

--44
partitionEithersd :: [Either a b] -> ([a],[b])
partitionEithersd (x:xs)
   | 

--45
