type Polinomio = [Monomio]
type Monomio = (Float, Int) 

type Mat a = [[a]]

--2)
--a)
selgrau :: Int -> Polinomio -> Polinomio 
--                    \padrão1 .. padrão n -> exp 
selgrau g p = filter  (\(a,b) -> b == g) p

{-
conta :: Int -> Polinomio -> Polinomio
conta g [] = 0
conta g((c,e):t)
   | g == e = 1 + conta g t
   | otherwise = conta g t			
-}
{-
Definição da funçao foldr 
foldr :: (a->b->b) -> b -> [a] -> b          |                     	 
foldr f c [] = c                             | Definição Recursiva  
foldr f c (x:xs) = f x (foldr f c xs)        |                   

Definição da função foldl                                
foldl :: (a->b->b) -> b -> [a] -> b          |                       
foldl f c [] = c                             | Definição Acumuladores        
foldl d c (x:xs) = foldl f (f c x) xs        |                    

-}



--foldr aplica da direita para a esquerda
--foldl aplica da esquerda para a direita 

--conta usando o foldr 

--contaf g p = foldr ()
--(\(c,e) d -> g == e them d+1 else d)
--conta 3 [(2,3,(1,1)] => foldr (\(c,e) d -> if e==g then d+1 else d ) 0 [(2,3),(1,1)] = >
--(\(c,e) d -> if e==g then d+1 else d ) (2,3) (foldr f 0 [(1,1)])
--if 3==3 then (foldr f 0 [(1,1)]) + 1 else foldr f 0 [(1,1)])
--foldr f 0 [(1,1)] + 1 => f (1,1) (foldr f 0 []) => 


--1)
--a) 
{-
anyd :: (a -> Bool) -> [a] -> Bool
anyd f [] = True
anyd f (x:xs) = if (f x) then True else f xs

--outra forma de resolver 


anydd :: (a -> Bool) -> [a] -> Bool
anydd f [] = True
anydd f (x:xs) = f x || anydd f xs

--c)
 -}
--MINHA RESOLUÇÃO

--1

ani :: (a -> Bool) -> [a] -> Bool
ani f [] = False
ani f (x:xs) = if f x == True then True else ani f xs 

zipweed :: (a->b->c) -> [a] -> [b] -> [c]
zipweed f (x:xs) (y:ys) = (f x y) : zipweed f xs ys 
zipweed f _ _ = []

takeWhil :: (a->Bool) -> [a] -> [a] 
takeWhil f [] = []
takeWhil f (x:xs)
    | f x == True = x:takeWhil f xs 
    | f x == False = []

dropWhil :: (a->Bool) -> [a] -> [a]
dropWhil f [] = []
dropWhil f (x:xs) 
    | f x == True = dropWhil f xs 
    | f x == False = xs

spand :: (a->Bool) -> [a] -> ([a],[a])
spand _ [] = ([],[])
spand f (x:xs) 
    | f x = (x:s1,s2)
    | otherwise = ([],x:xs)
    where (s1,s2) = spand f xs

deleteBys :: (a->a->Bool) -> a -> [a] -> [a] 
deleteBys f x (h:t) 
   | f x h == True = t
   | otherwise = h:deleteBys f x t

sortOn :: Ord b => (a->b) -> [a] -> [a]
sortOn f [] = [] 
sortOn f (x:xs) = insere (x) (sortOn f xs)
    where insere x [] = [x]
          insere x (a:b) = if f x > f a then a:insere x b else x:a:b 


--2



--slgrau x pol = filter (\(a,b) -> b == x) pol  
sellgrau :: Int -> Polinomio -> Polinomio
sellgrau y (x:xs) = filter (\(a,b) -> b == y ) (x:xs)

coonta :: Int -> Polinomio -> Int
coonta p (x:xs) = lengthlist (sellgrau p (x:xs))

lengthlist [] = 0
lengthlist (x:xs) = 1 + lengthlist xs

grau :: Polinomio -> Int
grau pl = foldl (\ acc x -> if acc > snd x then acc else snd x ) 0 pl


--que calcula a derivada de um polinomio.
deriv :: Polinomio -> Polinomio
deriv pl = filter (\(a,b) -> b /= 0 &&  a /= 0) (map mapderiv pl)
           

mapderiv :: Monomio -> Monomio
mapderiv (0,_) = (0,0)
mapderiv (_,0) = (0,0)
mapderiv (x,y) = (x * (fromIntegral (y)), y-1) 

--risingfisan
--deriv :: Polinomio -> Polinomio
--deriv ps = filter (/= (0,0)) $ map (\(b,e) -> if e /= 0 then (b * fromIntegral e, e - 1) else (0,0)) ps

--que calcula o valor de um polin´omiopara uma dado valor de x.

--calcula a = foldl (\ac (b,e) -> ac + b * (a ^ e)) 0
calcula :: Float -> Polinomio -> Float
calcula x = foldl(\ acc (a,b) -> acc + a * ( x ^ b)) 0 

--que retira de um polin´omio os mon´omios de coeficiente zero.
simp :: Polinomio -> Polinomio 
simp pol = filter (\ (a,b) -> a==0) pol

--que calcula o resultado da multiplica¸c˜ao de um mon´omio por um polin´omio.
{-
mult :: Monomio -> Polinomio -> Polinomio
mult (x,y) pol = map(\(a,b) -> (a*x,y+b))
-}
mult :: Monomio -> Polinomio -> Polinomio
mult (x,y) = map (\(b,e) -> (b*x,y+e))

--que ordena um polonómio por ordem crescente dos graus dos seus monómios.
ordena :: Polinomio -> Polinomio 
ordena = sortOn snd

--que dado um polinómio constrói um polinómio equivalente em que não podem aparecer varios monómios com o mesmo grau.
--normaliza :: Polinomio -> Polinomio
{-
normaliza [] =  
normaliza ((x,y):(xs,ys):pol) = 
-}

--	             --PERGUNTAR ISTO--
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((b,e):ps) = (sum [bs | (bs,es) <- selgrau e ps] + b,e):normaliza [(bo,eo) | (bo,eo) <- ps, eo /= e]


soma :: Polinomio -> Polinomio -> Polinomio
soma p r = normaliza $ (++) p r 

produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = foldl (\ac x -> soma (mult x p2) ac) [] p1


b=[[1,2,3], [0,4,5], [0,0,6]]

--3
--a)

dimOK :: Mat a -> Bool
dimOK (x:xs) = all (\a -> length x == length a) xs

dimMat :: Mat a -> (Int,Int)
dimMat m = (length m, length(head m)) 

addMat :: Num a => Mat a -> Mat a -> Mat a 
addMat (x:xs) (y:ys) = addMat 






