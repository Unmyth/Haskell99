data Quantity a = Single a | Multiple Int a deriving Show

decodeModifiedHelper :: [Quantity a] -> [a] -> [a]
decodeModifiedHelper [] ys = ys
decodeModifiedHelper ((Single z):xs) ys = decodeModifiedHelper xs (z:ys)
decodeModifiedHelper ((Multiple n z):xs) ys = decodeModifiedHelper xs ((take n $ repeat z) ++ ys)

decodeModified :: [Quantity a] -> [a]
decodeModified xs = reverse $ decodeModifiedHelper xs []
