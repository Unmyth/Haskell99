rotateHelper :: [a] -> [a] -> Int -> [a]
rotateHelper xs ys 0 = xs ++ reverse ys
rotateHelper [] ys _ = reverse ys
rotateHelper (x:xs) ys n = rotateHelper xs (x:ys) (n - 1)

rotate :: [a] -> Int -> [a]
rotate xs n | n > 0 = rotateHelper xs [] n
            | n == 0 = xs
            | otherwise = rotateHelper xs [] (length xs + n)
