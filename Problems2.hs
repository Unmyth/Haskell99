module Problems2
    where

import Test.QuickCheck 
import Control.Monad (liftM)
import System.Random (randomIO, randoms)
import Problems1 (removeAt, elementAt)
import System.Random (randomRIO)
import Test.QuickCheck.Monadic
import Data.List
import Data.Function (on)

-- Problem 21

insertAt :: a -> [a] -> Int -> [a]
insertAt e [] 1 = [e]
insertAt _ [] n | n > 1 = []
insertAt e xs 1 = e : xs
insertAt e (x:xs) n = x : insertAt e xs (n - 1)

testInsertAt = quickCheck $ \lst e (Small n) -> insertAt (e :: Int) lst (n + 1) == if n <= length lst 
                                                                                     then take n lst ++ [e] ++ drop n lst  
                                                                                     else lst

-- Problem 22

range :: Int -> Int -> [Int]
range a b | a < b = a : range (a + 1) b
          | a == b = [a]
          | a > b = []
range _ _ = []

newtype Small = Small Int
    deriving Show

instance Arbitrary Small where
    arbitrary = liftM Small $ choose (0, 30)

testRange = quickCheck $ \ (Small a) (Small b) -> range a b == [a .. b]

-- Problem 23

rndSelect :: [a] -> Int -> IO [a]
rndSelect lst num = inner lst num []
    where
      inner lst 0 acc = return acc
      inner lst num acc = do
            n <- randomRIO (1, length lst)
            let (e, lst1) = removeAt n lst
            inner lst1 (num - 1) (e : acc)

testRndSelect = quickCheck $ monadicIO $ do
                  (NonEmpty origLst) <- pick arbitrary
                  n <- pick $ choose (1, length origLst)
                  lst <- run $ rndSelect (origLst :: [Char]) n
                  assert $ length lst == n
                  assert $ all (\ e -> elem e origLst) lst
                  assert $ all (\ e -> (length $ elemIndices e lst) <= (length $ elemIndices e origLst)) lst

-- Problem 24

diffSelect :: Int -> Int -> IO [Int]
diffSelect num maxN = rndSelect [1..maxN] num

testDiffSelect = quickCheck $ monadicIO $ do
                  maxN <- pick $ choose (1, 100)
                  n <- pick $ choose (1, 10 `min` maxN)
                  lst <- run $ diffSelect n maxN
                  assert $ length lst == n
                  assert $ all (\ e -> e <= maxN) lst
                  assert $ all (\ e -> (length $ elemIndices e lst) == 1) lst

-- Problem 25

rndPermu :: [a] -> IO [a]
rndPermu lst = rndSelect lst (length lst)

testRndPermu = quickCheck $ monadicIO $ do
                 (NonEmpty origLst) <- pick arbitrary
                 lst <- run $ rndPermu (origLst :: [Char])
                 assert $ length lst == length origLst
                 assert $ all (\ e -> (length $ elemIndices e lst) == (length $ elemIndices e origLst)) lst
                 
-- Problem 26

subElems :: [a] -> Int -> [(a, [a])]
subElems lst minLen = inner lst (length lst - minLen + 1)
    where inner _ 0 = [] 
          inner (x:xs) n = (x, xs) : inner xs (n - 1)

numComb :: Int -> Int -> Int
numComb n k = div (product [n-k+1..n]) (product [1..k])

combinations :: Int -> [a] -> [[a]]
combinations 1 lst = [[x] | x <- lst]
combinations n lst = do
  (el, lst1) <- subElems lst n
  subComb <- combinations (n - 1) lst1
  return $ el : subComb

testCombinations = quickCheck $ forAll (choose (5, 10)) $
                    \ lstLen -> forAll (choose (1, 3)) $
                    \ num -> let lst = [1..lstLen]
                                 combs = combinations num (lst :: [Int])
                             in length combs == numComb (length lst) num &&
                                (all (\ e -> (length $ findIndices (\ e2 -> sort e == sort e2) combs) == 1) combs)

-- Problem 27

subElemsR :: [a] -> Int -> [(a, [a], [a])]
subElemsR lst minLen = inner lst (length lst - minLen + 1) []
    where inner _ 0 _ = [] 
          inner (x:xs) n acc = (x, xs, acc) : inner xs (n - 1) (x:acc)

combinationsR :: Int -> [a] -> [([a], [a])]
combinationsR 1 lst = do
  (el, others, prev) <- subElemsR lst 1
  return $ ([el], reverse prev ++ others)
combinationsR n lst = do
  (el, lst1, prev1) <- subElemsR lst n
  (subComb, others) <- combinationsR (n - 1) lst1
  return $ (el : subComb, reverse prev1 ++ others)

multinomCoef :: Int -> [Int] -> Int
multinomCoef n ks = (product [1 .. n]) `div` (product $ map (\k -> product [1..k]) ks)

groups :: [Int] -> [a] -> [[[a]]]
groups [num] lst = return $ [lst]
groups (num : nums) lst = do
  (gr, others) <- combinationsR num lst
  otherGroups <- groups nums others
  return $ gr : otherGroups

testGroups = quickCheck $ forAll (choose (1, 3)) $
             \ numGroups -> forAll (vectorOf numGroups (choose (1,3))) $
             \ groupNums -> let lst = [1 .. sum groupNums]
                                grs = groups groupNums lst
                            in length grs == multinomCoef (sum groupNums) groupNums

-- Problem 28

splitBy :: [a] -> (a -> Bool) -> ([a], [a])
splitBy [] _ = ([], [])
splitBy (x:xs) pred = let ~(l1, l2) = splitBy xs pred
                      in if pred x
                           then (x : l1, l2)
                           else (l1, x : l2)

mySortBy :: [a] -> (a -> Int) -> [a]
mySortBy [] _ = []
mySortBy [e] _ = [e]
mySortBy (x:xs) key = let xkey = key x
                          ~(l1 ,l2) = splitBy xs (\el -> key el <= xkey)
                      in mySortBy l1 key ++ [x] ++ mySortBy l2 key

lsort :: [[a]] -> [[a]]
lsort lst = mySortBy lst length

testSort sortFun orderFun = quickCheck $ \ lst -> let sorted = sortFun (lst :: [String])
                                                  in length sorted == length lst &&
                                                     and (zipWith ((<=) `on` (orderFun lst)) sorted (tail sorted)) &&
                                                     all (\e -> elem e sorted) lst &&
                                                     all (\e -> elem e lst) sorted

testLSort = testSort lsort (\ lst el -> length el)

countElems :: Eq a => a -> [a] -> Int
countElems elem lst = inner lst elem 0
    where inner [] _ cnt = cnt
          inner (x:xs) el cnt | el == x = inner xs el (cnt + 1)
                              | otherwise = inner xs el cnt

lfsort :: Eq a => [[a]] -> [[a]]
lfsort lst = let withCounts = map (\ l -> (countElems l lst, l)) lst
                 sorted = mySortBy withCounts fst
             in map snd sorted

testLFSort = testSort lfsort (\ lst el -> countElems el lst)



