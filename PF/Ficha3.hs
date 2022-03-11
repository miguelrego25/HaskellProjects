data Hora = H Int Int
         deriving Show
type Etapa = (Hora,Hora)
type Viagem = [Etapa]


horaDepois :: Hora -> Hora -> Bool
horaDepois (H h1 m1) (H h2 m2) = h1 > h2 || (h1 == h2 && m1 > m2)

--a
etapaBemConstruida :: Etapa -> Bool 
etapaBemConstruida (a,b)
   |horaDepois b a = True
   |otherwise = False

--b
{-
viagemBemConstruida :: Viagem -> Bool
viagemBemConstruida [] = []
viagemBemConstruida [x] = True
viagemBemConstruida ((a1,b1):(a2,b2):xs)
   |etapaBemConstruida (a1,b1) && horaDepois (a1,b1) &&  horaDepois b1 a2= viagemBemConstruida((a2,b2):xs)
   |otherwise = False
-}
--b REsoluçao do stor 
viagemValida:: Viagem -> Bool
viagemValida [e] = etapaBemConstruida e
viagemValida ((h1,h2):xs) = etapaBemConstruida(h1,h2)&& horaDepois(fst( head (xs))) h2 && viagemValida xs