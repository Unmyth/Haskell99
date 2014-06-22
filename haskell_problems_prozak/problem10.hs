encodeHelper :: (Eq a) => [a] -> [(Int, a)] -> [(Int, a)]
encodeHelper [] ys = ys
encodeHelper (x : xs) [] = encodeHelper xs [(1, x)]
encodeHelper (x : xs) (pair@(n, y):pairs) | x == y = encodeHelper xs (((n + 1), y) : pairs)
                                    | otherwise = encodeHelper xs ((1, x) : pair : pairs)

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = reverse $ encodeHelper xs []
