import Data.Char 

digitAlpha :: String -> (String,String)
digitAlpha [] = ([], [])
digitAlpha (x:xs) 
   | isDigit x = (x:a,b)
   | isAlpha x = (a, x:b)
   | otherwise = (a,b)
      where (a,b) = digitAlpha xs

--4
data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)

firstSeq :: Seq a -> a 
firstSeq Cons a s = a
firstSeq App Nil s2 = firstSeq s2 
firstSeq App s1 _ = firstSeq s1 

--5 
type Mat a = [[a]] 

getElem :: Mat a -> IO a
getElem mat = let (l,c) = (length mat, length (head c))
        linhaale <- randomRIO(0,l-1)
        colunasale  <- randomRIO(0,c-1)
        return ((mat !! linhaale) !! colunasale) 

