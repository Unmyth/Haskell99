myReverseHelper :: [a] -> [a] -> [a]
myReverseHelper [] ys = ys
myReverseHelper (x : xs) ys = myReverseHelper xs (x : ys)

myReverse :: [a] -> [a]
myReverse xs = myReverseHelper xs []
