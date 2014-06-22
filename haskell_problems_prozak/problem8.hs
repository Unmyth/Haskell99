compressHelper :: (Eq a) => [a] -> [a] -> [a]
compressHelper [] ys = ys
compressHelper (x : xs) [] = compressHelper xs [x]
compressHelper (x : xs) ys@(y : _) | x == y = compressHelper xs ys
                                 | otherwise = compressHelper xs (x : ys)

compress :: (Eq a) => [a] -> [a]
compress xs = reverse $ compressHelper xs []
