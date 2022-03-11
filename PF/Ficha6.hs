data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

umaarvorer = (Node 5 (Node 2 Empty (Node 3 Empty Empty))  (Node 9 (Node 7 (Node 6 Empty Empty) (Node 8 Empty Empty)) Empty ))
umaarvore = (Node 5 (Node 2 (Node 1 Empty Empty)(Node 3 Empty Empty))  (Node 9 (Node 7 (Node 6 Empty Empty) (Node 8 Empty Empty)) Empty )) 
segundaarvore = (Node 5 (Node 2 Empty Empty) (Node 3 Empty Empty))

altura :: BTree a -> Int 
altura Empty = 0
altura (Node r e d) = max (1+altura e) (1+altura d)

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node r e d) = 1 + contaNodos e + contaNodos d 

folhas :: BTree a -> Int 
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node r e d ) = folhas e + folhas d

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty
prune a (Node r e d)  = (Node r (prune (a-1) e) (prune (a-1) d))

path :: [Bool] -> BTree a -> [a]
path [] _= []
path _ Empty = []
path (x:xs) (Node e l r) 
   | x == False = e: (path xs l)
   | x == True = e: (path xs r)  

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node e l r) = (Node e (mirror r) (mirror l))

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT function Empty Empty = Empty
zipWithBT function Empty a = Empty
zipWithBT function a Empty = Empty
zipWithBT function (Node e l r) (Node es ls rs) = (Node (function e es) (zipWithBT function l ls) (zipWithBT function r rs))

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT (Node (a,b,c) l r) = (Node a unzipL1 unzipR1,Node b unzipL2 unzipR2,Node c unzipL3 unzipR3)
    where (unzipL1,unzipL2,unzipL3) = unzipBT l
          (unzipR1,unzipR2,unzipR3) = unzipBT r

minimo :: Ord a => BTree a -> a
minimo (Node e Empty r) = e
minimo (Node e l r) = minimo l

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node e Empty _) = Empty
semMinimo (Node e l r) = (Node e (semMinimo l) r) 

minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node e Empty _) = (e, Empty) 
minSmin (Node e l r) = (a,Node e b r)
    where (a,b) = minSmin l 

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node e l r) | x < e = Node e (remove x l) r
                      | x > e = Node e l (remove x r)
                      | otherwise = aux x (Node e l r)
    where aux n (Node a b c) = case b of Empty -> c
                                         otherwise -> case c of Empty -> b
                                                                otherwise -> Node g b h
          (g,h) = minSmin r

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL  deriving Show
data Classificacao = Aprov Int | Rep | Faltou deriving Show
type Turma = BTree Aluno  --  árvore binária de procura (ordenada por número)

turma1 :: Turma
turma1 = (Node (15,"Luís",ORD,Aprov 14) (Node (12,"Joana",MEL,Faltou) (Node (7,"Diogo",TE,Rep) Empty
                                                                                               Empty) 
                                                                      (Node (14,"Lara",ORD,Aprov 19) Empty
                                                                                                     Empty))
                                        (Node (20,"Pedro",TE,Aprov 10) Empty
                                                                       (Node (25,"Sofia",ORD,Aprov 20) (Node (23,"Rita",ORD,Aprov 17) Empty
                                                                                                                                      Empty)
                                                                                                       (Node (28,"Vasco",MEL,Rep) Empty
                                                                                                                                  Empty))))

inscNum :: Numero -> Turma -> Bool
inscNum x Empty = False 
inscNum x (Node (e,nome,regime,cl) r l) 
   |x == e = True
   |e > x = inscNum x r
   |e < x = inscNum x l



inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome nome1 (Node (numero,nome2,_,_) l r)
   | nome1 == nome2 = True
   | otherwise = inscNome nome1 l || inscNome nome1 r 
{-
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (numero,nome,trab,_) l r)
   |TE == trab = [(numero,nome)] ++ trabEst l ++ trabEst r
   |otherwise = trabEst l ++ trabEst r 
-}

nota :: Numero -> Turma -> Maybe Classificacao
nota numero Empty = Nothing 
nota numero (Node (num,nome,reg,classi) l r) 
   | numero == num = Just classi
   | numero > num = nota numero r
   | numero < num = nota numero l 
nota _ _ = Nothing
{-
percFaltas :: Turma -> Float
percFaltas turma = faltou turma / total turma * 100 
   where faltou Empty = 0
         faltou (Node (_,_,_,classi) l r)
            | classi == Faltou = 1 + faltou l + faltou r
            | otherwise = faltou l + faltou r 
         total (Node (_,_,_,_) l r) = 1+ total l + total r 
         total Empty = 0
-}

mediaAprov :: Turma -> Float
mediaAprov turma = total turma / npessoas turma  
   where total (Node (_,_,_,Aprov x) l r) = fromIntegral (x) + total l + total r
         total (Node (_,_,_,_) l r) = total l + total r 
         total Empty = 0  
         npessoas (Node (_,_,_,Aprov x) l r) = fromIntegral(1) + npessoas l + npessoas r
         npessoas (Node (_,_,_,_) l r) = npessoas l + npessoas r
         npessoas Empty = 0

aprovAv :: Turma -> Float 


