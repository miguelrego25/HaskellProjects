elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices a (x:xs) = elemIndicess 0 a (x:xs)

elemIndicess :: Eq a => a -> a -> [a] -> [Int]
elemIndicess _ _ [] = [] 
elemIndicess p a (x:xs)
   | a == x = p:elemIndicess p+1 a xs
   | otherwise = elemIndicess p+1 a xs