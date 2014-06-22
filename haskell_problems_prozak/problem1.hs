myLast :: [a] -> a
myLast [] = error "myLast: empty list"
myLast xs = head $ reverse xs

myLastTest :: (Eq a) => [a] -> Bool
myLastTest [] = True
myLastTest xs = myLast xs == last xs
