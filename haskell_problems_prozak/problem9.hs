packHelper :: (Eq a) => [a] -> [[a]] -> [[a]]
packHelper [] ys = ys
packHelper (x : xs) [] = packHelper xs [[x]]
packHelper (x : xs) (zs'@(z:zs):ys) | x == z = packHelper xs ((x : zs') : ys)
                                    | otherwise = packHelper xs ([x] : zs' : ys)

pack :: (Eq a) => [a] -> [[a]]
pack xs = reverse $ packHelper xs []

packTest xs = True
