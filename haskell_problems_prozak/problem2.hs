myButLast :: [a] -> a
myButLast [] = error "myButLast: list is to small"
myButLast [x] = myButLast []
myButLast xs = head $ tail $ reverse xs
