intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect (x:xs) (y:ys)
   |elem x (y:ys) = x:intersect xs (y:ys)
   |otherwise = intersect xs (y:ys)

tails :: [a] -> [[a]]
tails [] = [[]]
tails [x] = [[x]]
tails (x:xs) = (x:xs) : tails (tail (x:xs))

type ConjInt = [Intervalo]
type Intervalo = (Int,Int)

elems :: ConjInt -> [Int]
elems [] = []
elems ((a,b):xs)
   | a /= b = a: elems (((a+1),b):xs)
   | a == b = b: elems xs

checkstop :: [Int] -> Int 
checkstop [x] = x
checkstop (x:xs:xss)
   | x == (xs-1) = checkstop (xs:xss)
   | otherwise = xs   

--geraconj :: [Int] -> ConjInt
--geraconj (x:xs:xss)
  -- | x == (xs-1) = (x,checkstop (xs:xss)):
  -- | otherwise =  



x = [(1,4),(7,8),(19,19),(21,23)]
--3

data Contacto = Casa Integer
    | Trab Integer
    | Tlm Integer
    | Email String
   deriving (Show)

type Nome = String
type Agenda = [(Nome, [Contacto])]

{-acrescEmail nome email [] = [(nome,[Email email])]
acrescEmail nome email (((nomex),contactos):t) 
   | nome == nomex = ((nomex), (Email email):contactos):t
   | otherwise = head agenda :acrescEmail nome email t
-}
procuramail :: Contacto -> [String] 
procuramail ((Email x):xs) = x: procuramail xs  
procuramail x = procuramail x 


verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome ((nomea,contacto):t)
   |nome == nomea = Just (procuramail contacto)
   | otherwise = verEmails nome t 
