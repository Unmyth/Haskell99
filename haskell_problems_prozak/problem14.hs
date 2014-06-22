dupli :: [a] -> [a]

dupli xs = foldr (\x xs -> x : x : xs) [] xs
