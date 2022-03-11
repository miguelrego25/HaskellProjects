{-
retiraelem _ [] = []
retiraelem (x:xs) y 
   | x == y = xs
   | otherwise = x:retiraelem xs y

retira :: Eq a => [a] -> [a] -> [a]
retira l [] = l  
retira (x:xs) (y:ys)
   | elem x (y:ys) = retira (retiraelem x (y:ys)) xs 
   | otherwise = retira (x:xs) (ys)
-}
type MSet a = [(a,Int)]

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a [] = [] 
removeMSet x ((a,b):resto) 
   | x == a = resto
   | otherwise = (a,b): removeMSet x resto 

calcula :: MSet a -> ([a],Int)
calcula = foldr (\x (ls,ns) -> ((fst x):ls ,(snd x)+ns)) ([],0)
