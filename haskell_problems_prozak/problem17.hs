splitHelper :: [a] -> [a] -> Int -> ([a],[a])
splitHelper [] ys _ = (reverse ys, [])
splitHelper xs ys 0 = (reverse ys, xs)
splitHelper (x:xs) ys n = splitHelper xs (x:ys) (n - 1)

split :: [a] -> Int -> ([a],[a])
split xs n = splitHelper xs [] n
