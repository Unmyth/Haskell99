data Quantity a = Single a | Multiple Int a deriving Show

encodeModifiedHelper :: (Eq a) => [a] -> [Quantity a] -> [Quantity a]
encodeModifiedHelper [] ys = ys
encodeModifiedHelper (x : xs) [] = encodeModifiedHelper xs [Single x]
encodeModifiedHelper (x : xs) (y@(Single z):ys)
    | x == z = encodeModifiedHelper xs ((Multiple 2 x) : ys)

encodeModifiedHelper (x : xs) (y@(Multiple n z):ys)
    | x == z = encodeModifiedHelper xs ((Multiple (n + 1) x) : ys)

encodeModifiedHelper (x : xs) ys = encodeModifiedHelper xs ((Single x) : ys)

encodeModified :: (Eq a) => [a] -> [Quantity a]
encodeModified xs = reverse $ encodeModifiedHelper xs []
