isSorted :: (Ord a) => [a] -> Bool 
isSorted [] = True 
isSorted [x] = True 
isSorted (x:xs:xss) 
   |x <= xs = isSorted (xs:xss)
   |otherwise = False

inits :: [a] -> [[a]] 
inits [] = [[]]
inits l = inits (init l) ++ [l] 

maximumMB :: (Ord a) => [Maybe a] -> Maybe a
maximumMB = foldr (\x acc -> if maiormaybe x acc then x else acc ) Nothing
    where maiormaybe (Just a) (Just b) = a>b 
          maiormaybe Nothing _ = False 
          maiormaybe _ Nothing = True 

data LTree a = Tip a | Fork (LTree a) (LTree a)

listaLT :: LTree a -> [a] 
listaLT (Tip a) = [a]
listaLT (Fork a b) = listaLT a ++ listaLT b

instance (Show a) => Show(LTree a) where
	show (Tip a) = show a 
	show (Fork a b) = pontinhos 1 a ++ pontinhos 1 b

pontinhos :: (Show a) => Int -> LTree a -> String
pontinhos 0 (Tip a) = show (a)
pontinhos n (Tip a) = "." ++ pontinhos n-1 (Tip a)
pontinhos n (Fork a b) = pontinhos (n+1) a ++ pontinhos (n+1) b 

--maxSumInit :: (Num a, Ord a) => [a] -> a
--maxSumInit 

maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maximum [sum m | m <- inits l]

maxSumInit ::  (Num a, Ord a) => [a] -> a
maxSumInit [] = 0
maxSumInit (x:xs) = aux' xs x x

aux' :: (Num a, Ord a) => [a] -> a -> a -> a
aux' [] m s = m
aux' (x:xs) m s | (m >= s+x) = aux' xs m (s+x)
                | otherwise  = aux' xs (s+x) (s+x)