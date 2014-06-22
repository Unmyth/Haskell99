elementAt :: [a] -> Int -> a
elementAt [] n = error $ "elementAt: elm index is out of bounds"
elementAt (x : xs) 1 = x
elementAt (x : xs) n = elementAt xs (n - 1)

elementAtTest xs = elementAt xs 3 == xs !! 3
