sliceHelper :: [a] -> Int -> Int -> [a] -> [a]
sliceHelper [] _ _ ys = ys
sliceHelper xs 1 0 ys = ys
sliceHelper (x:xs) 1 k ys = sliceHelper xs 1 (k - 1) (x:ys)
sliceHelper (x:xs) i k ys = sliceHelper xs (i - 1) k ys

slice :: [a] -> Int -> Int -> [a]
slice xs i k = reverse $ sliceHelper xs i (k - i + 1) []
