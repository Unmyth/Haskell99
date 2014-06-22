removeAtHelper :: Int -> [a] -> [a] -> (a, [a])
removeAtHelper _ [] ys = error "removeAtHelper: elem index is out of bounds"
removeAtHelper 1 (x:xs) ys = (x, reverse ys ++ xs)
removeAtHelper i (x:xs) ys = removeAtHelper (i - 1) xs (x:ys)

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = removeAtHelper n xs []
