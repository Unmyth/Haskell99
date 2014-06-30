module Problems3 
    where

import Test.QuickCheck 
import Data.List
import Debug.Trace (trace)

-- Problem 31

maxDivisor :: Integral a => a -> a
maxDivisor n = floor $ sqrt $ fromIntegral n

isPrime :: Integral a => a -> Bool
isPrime n = let maxDiv = maxDivisor n
            in all (\ k -> n `mod` k /= 0) [2..maxDiv]

-- Problem 32

myGCD :: Integral a => a -> a -> a
myGCD a 0 = a
myGCD a b = myGCD b (a `mod` b)

testGCD = quickCheck $ \ (NonNegative a) (NonNegative b) -> gcd (a :: Int) b == myGCD a b

-- Problem 33

coprime :: Integral a => a -> a -> Bool
coprime a b = myGCD a b == 1

testCoprime = quickCheck $ \ (NonNegative a) (NonNegative b) -> (gcd (a :: Int) b == 1) == coprime a b

-- Problem 34

totient :: Integral a => a -> Int
totient 1 = 1
totient n = length $ filter (coprime n) [1 .. n]

-- Problem 35

primes :: Integral a => [a]
primes = 2:3:filter (\n -> let maxDiv = maxDivisor n
                           in all (\m -> n `mod` m /= 0) $ takeWhile (<= maxDiv) primes)
             [5, 7 ..] 

primeFactors :: Integral a => a -> [a]
primeFactors n = inner n (takeWhile (<= (maxDivisor n)) primes)
    where inner n primesToMaxDiv = case find (\m -> n `mod` m == 0) primesToMaxDiv of
                                     Just prime -> prime : inner (n `div` prime) primesToMaxDiv
                                     Nothing    -> if n == 1
                                                     then []
                                                     else [n]

testPrimeFactors = quickCheck $ \ (NonNegative a) -> let divs = primeFactors (a :: Integer)
                                                     in all isPrime divs &&
                                                        product divs == a

-- Problem 36

primeFactorsMult :: Integral a => a -> [(a, Int)]
primeFactorsMult n = inner n (takeWhile (<= (maxDivisor n)) primes)
    where primesMult p i n = if n `mod` p == 0
                               then primesMult p (i + 1) (n `div` p)
                               else (i, n)
          inner n primesToMaxDiv = case find (\m -> n `mod` m == 0) primesToMaxDiv of
                                     Just prime -> let (numP, newN) = primesMult prime 0 n
                                                   in (prime, numP) : inner newN primesToMaxDiv
                                     Nothing    -> if n == 1
                                                     then []
                                                     else [(n, 1)]

testPrimeFactorsMult = quickCheck $ \ (NonNegative a) -> let divs = primeFactorsMult (a :: Integer)
                                                         in all isPrime (map fst divs) &&
                                                            product (map (\ (a, n) -> a ^ n) divs) == a


-- Problem 37

totient2 :: Integral a => a -> a
totient2 1 = 1
totient2 n = let divs = primeFactorsMult n
             in product $ map (\ (p, m) -> (p - 1) * p ^ (m - 1)) divs


testTotient = quickCheck $ forAll (choose (10, 100000)) $ \ a -> totient a == totient2 (a :: Int)

-- Problm 39

primesR :: Integral a => a -> a -> [a]
primesR a b = dropWhile (< a) $ takeWhile (<= b) $ primes

-- Problem 40

data LZipper a = LZipper { zLeft :: [a], 
                           zHead :: a,
                           zRight :: [a]
                         }

instance Show a => Show (LZipper a) where
    show (LZipper l h r) = "(LZipper " ++ show (take 3 l) ++ " " ++ show h ++ " " ++ show (take 3 r) ++ ")"

shiftR' :: LZipper a -> (LZipper a, Bool)
shiftR' (LZipper l1 x (y:ys)) = (LZipper (x:l1) y ys, True)
shiftR' z@(LZipper _ _ []) = (z, False)

shiftL' :: LZipper a -> (LZipper a, Bool)
shiftL' (LZipper (y:ys) x l1) = (LZipper ys y (x:l1), True)
shiftL' z@(LZipper [] _ _) = (z, False)

shiftR :: LZipper a -> LZipper a
shiftR z = fst $ shiftR' z

shiftL :: LZipper a -> LZipper a
shiftL z = fst $ shiftL' z

shiftWhile :: LZipper a -> (LZipper a -> (LZipper a, Bool)) -> (a -> Bool) -> LZipper a
shiftWhile zipper shiftFunc pred = if not (pred $ zHead zipper)
                                   then zipper
                                   else let (newZ, continue) = (shiftFunc zipper)
                                        in if continue
                                             then shiftWhile newZ shiftFunc pred
                                             else newZ

listZipper :: [a] -> LZipper a
listZipper (x:xs) = LZipper [] x xs

primesZipper :: Integral a => LZipper a
primesZipper = listZipper primes

-- goldbachFind :: (Show a, Integral a) => a -> LZipper a -> LZipper a -> Maybe (LZipper a, LZipper a)
-- goldbachFind num lz rz = trace ("Num = " ++ show num ++ ", L = " ++ show lz ++ ", R = " ++ show rz) $
--                          goldbachFind' num lz rz

goldbachFind :: (Show a, Integral a) => a -> LZipper a -> LZipper a -> Maybe (LZipper a, LZipper a)
goldbachFind num lz@LZipper{zHead = lh} rz@LZipper{zHead = rh} | lh == rh = goldbachFind num lz (shiftR rz)
                                                               | lh + rh == num = Just (lz, rz)
                                                               | lh + rh < num = goldbachFind num lz (shiftR rz)
                                                               | lh > num = Nothing
                                                               | otherwise = let newLZ@LZipper{zHead = newLH} = (shiftR lz)
                                                                                 newStart = \ x -> x + newLH > num
                                                                             in goldbachFind num newLZ (shiftWhile rz shiftL' newStart)

-- goldbach :: Integral a => a -> (a, a)
-- goldbach n = inner $ tail primes
--     where inner (p : otherPrimes) = case find (\x -> x + p == n) $ takeWhile (<= (n - p)) otherPrimes of
--                                       Just p2 -> (p, p2)
--                                       Nothing -> inner otherPrimes

goldbach :: (Show a, Integral a) => a -> (a, a)
goldbach n = let (Just (lz, rz)) = goldbachFind n primesZipper primesZipper
             in (zHead lz, zHead rz)

testGoldbach = quickCheck $ forAll (choose (10, 100000)) $ \ x' -> let x = x' * 2
                                                                       (a, b) = goldbach (x :: Integer)
                                                                   in isPrime a && isPrime b && a + b == x
                                                                     

-- Problem 41


goldbachList :: (Show a, Integral a) => a -> a -> [(a, a)]
goldbachList a b | a > b = []
                 | even a = goldbach a : goldbachList (a + 1) b
                 | otherwise = goldbachList (a + 1) b

goldbachList2' :: (Show a, Integral a) => a -> a -> (LZipper a -> LZipper a) -> LZipper a -> LZipper a -> [(a, a)]
goldbachList2' a b resetLZ lz rz | a > b = []
                                 | even a = case goldbachFind a lz rz of
                                              Just (newLZ, newRZ) -> (zHead newLZ, zHead newRZ) : goldbachList2' (a + 1) b resetLZ (resetLZ newLZ) newRZ
                                              Nothing -> goldbachList2' (a + 1) b resetLZ lz rz
                                 | otherwise = goldbachList2' (a + 1) b resetLZ lz rz
                                       
goldbachList2 :: (Show a, Integral a) => a -> a -> [(a, a)]
goldbachList2 a b = let primesG = primesZipper
                    in goldbachList2' a b (const primesG) primesG primesG


goldbachList2Filtered :: (Show a, Integral a) => a -> a -> a -> [(a, a)]
goldbachList2Filtered a b minP = filter (\ (a, b) -> a >= minP && b >= minP) $ goldbachList2 a b

testGoldbachList = quickCheck $ forAll (choose (10, 1000)) $ 
                   \ a -> forAll (choose (10, 1000)) $ 
                   \ b -> let gl = goldbachList2 a (a + b)
                          in and (zipWith (\ (p1,p2) x -> isPrime (p1 :: Integer) && isPrime p2 && p1 + p2 == x) gl $ filter even [a.. (a+b)]) 
                            
