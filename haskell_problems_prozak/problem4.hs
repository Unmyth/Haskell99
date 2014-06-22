myLengthHelper :: [a] -> Int -> Int
myLengthHelper [] n = n
myLengthHelper (x : xs) n = myLengthHelper xs (n + 1)

myLength :: [a] -> Int
myLength xs = myLengthHelper xs 0
