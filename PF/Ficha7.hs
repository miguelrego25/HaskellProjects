
data ExpInt = Const Int
          | Simetrico ExpInt
          | Mais ExpInt ExpInt
          | Menos ExpInt ExpInt
          | Mult ExpInt ExpInt

p =infixa (Mais (Const 3) (Menos (Const 2) (Const 5)))

calcula :: ExpInt -> Int
calcula (Const num ) = num
calcula (Simetrico a) =  (-calcula a)
calcula (Mais a b) = calcula a + calcula b
calcula (Menos a b) = calcula a - calcula b
calcula (Mult a b) = calcula a * calcula b 

infixa :: ExpInt -> String
infixa (Const x )= show x
infixa (Simetrico exp) = "( - " ++ infixa exp ++ ")"
infixa (Mais a b) = "( " ++ infixa a ++ " + " ++ infixa b ++ ")"
infixa (Menos a b) = "(" ++ infixa a ++ " - " ++ infixa b ++ ")"
infixa (Mult a b) = "(" ++ infixa a ++ " * " ++ infixa b ++ ")" 

posfixa :: ExpInt -> String
posfixa (Const x) = show x
posfixa (Simetrico exp) = "-" ++ posfixa exp
posfixa (Mais a b) = posfixa a ++ " " ++ posfixa b ++ " +"
posfixa (Menos a b) = posfixa a ++ " " ++ posfixa b ++ " -"
posfixa (Mult a b) = posfixa a ++ " " ++ posfixa b ++ " *"

--2
data RTree a = R a [RTree a]
rtree1 = R 6 [R 4 [R 7 [R 1 [],
                        R 3 []],
                   R 9 []],
              R 3 [R 12 []],
              R 6 [],
              R 11 []]

soma :: Num a => RTree a -> a
soma (R a []) = a
soma (R a resto) = a + sum (map (soma) resto)

altura :: RTree a -> Int
altura (R a []) = 1
altura (R a as) = 1 + maximum ( map altura as) 

prune :: Int -> RTree a -> RTree a
prune 0 (R a as) = R a []
prune x (R a as) = R a (map (prune (x-1)) as)

mirror :: RTree a -> RTree a
mirror (R a []) = R a []
mirror (R a as) = R a (map mirror(reverse as))

--3

data BTree a = Empty | Node a (BTree a) (BTree a)

ltSum :: Num a => LTree a -> a 
ltSum Tip a = a 
ltSum Node a b = ltSum a + ltSum b

listaLT :: LTree a -> [a] 
listaLT 
listaLT Tip a = [a]
listaLT Node a b = listaLT a ++ listaLT

ltHeight :: LTree a -> Int
ltHeight Tip a = 1
ltHeight Node a b = 1+max (ltHeight a) (ltHeight b) 

--5

data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

splitFTree :: FTree a b -> (BTree a, LTree b) 
splitFTree No x a b = ()






