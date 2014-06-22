repliHelper :: a -> [a] -> Int -> [a]
repliHelper x xs 0 = xs
repliHelper x xs n = repliHelper x (x:xs) (n - 1)

repli :: [a] -> Int -> [a]
repli xs n = foldr (\x ys -> repliHelper x ys n) [] xs
