dropEveryHelper :: [a] -> [a] -> Int -> Int -> [a]
dropEveryHelper [] ys _ _ = ys
dropEveryHelper (x:xs) ys origN 0 = dropEveryHelper xs ys origN origN
dropEveryHelper (x:xs) ys origN n = dropEveryHelper xs (x:ys) origN (n - 1)

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = reverse $ dropEveryHelper xs [] n n
