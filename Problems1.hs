{-# LANGUAGE BangPatterns, DeriveDataTypeable #-}
module Problems1
    where

import Test.QuickCheck
import Data.Generics
import Data.Generics.Schemes
import Data.List

myLast :: [a] -> a
myLast [] = error "Empty list in myLast"
myLast [x] = x
myLast (_:xs) = myLast xs

testLast = quickCheck (\l -> myLast (l ++ [10]) == (10 :: Int))

myButLast :: [a] -> a
myButLast [] = error "Empty list in myButLast"
myButLast [_] = error "Singleton list in myButLast"
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

testButLast = quickCheck (\l -> myButLast (l ++ [10,20]) == (10 :: Int))

elementAt :: [a] -> Int -> a
elementAt [] _ = error "List too short in elementAt"
elementAt (x:xs) 1 = x
elementAt (_:xs) n = elementAt xs (n - 1)

testElementAt = quickCheck $ forAll (suchThat arbitrary $ \ lst -> length lst >= 1) $
                             \ lst -> forAll (choose (0, length lst - 1)) $
                             \ num -> elementAt (lst :: [Int]) (num + 1) == lst !! num


myLength :: [a] -> Int
myLength lst = calc lst 0
    where calc [] !len = len
          calc (_:xs) !len = calc xs (len + 1)

testMyLength = quickCheck $ \ lst -> length (lst :: [Int]) == myLength lst 

myReverse :: [a] -> [a]
myReverse lst = rev lst []
    where rev [] !lst = lst
          rev (x:xs) !lst = rev xs (x:lst)

testMyReverse = quickCheck $ \ lst -> myReverse (lst :: [Int]) == reverse lst

isPalindrome :: Eq a => [a] -> Bool
isPalindrome lst = lst == myReverse lst

testIsPalindrome = do
  quickCheck $ \ lst -> isPalindrome ((lst :: [Int]) ++ reverse lst)
  quickCheck $ forAll (suchThat arbitrary (\lst -> not (null lst))) $
                 \ lst -> isPalindrome ((lst :: [Int]) ++ tail (reverse lst))
  quickCheck $ forAll (suchThat arbitrary (\lst -> length lst >= 2)) $
                 \ lst -> forAll (suchThat arbitrary (\ e -> not $ elem (e :: Int) lst)) $
                 \ e -> not $ isPalindrome $ e : lst

data NestedList a = Elem a | List [NestedList a]
                    deriving (Show, Data, Typeable)

flatten :: NestedList a -> [a]
--flatten (Elem a) = [a]
--flatten (List lsts) = concatMap flatten lsts
flatten nlst = inner [nlst] []
    where 
      inner [] [] = []
      inner [] (x : xs) = inner x xs
      inner ((Elem a) : elems) lst = a : inner elems lst
      inner ((List l1) : elems) lst = inner l1 (elems : lst)

instance Arbitrary a => Arbitrary (NestedList a) where
    arbitrary = sized nlist
        where nlist 0 = do 
                v <- arbitrary
                return $ Elem v
              nlist n | n > 0 = oneof [ do
                                         v <- arbitrary
                                         return $ Elem v 
                                      ,
                                        do
                                          numEl <- choose (1, 4)
                                          els <- mapM (\ _ -> nlist $ (n - 1) `div` numEl) [0..numEl]
                                          return $ List els]

testFlatten = quickCheckWith stdArgs{maxSize = 100} $ \ nlst -> flatten (nlst :: NestedList Int) == listify (const True) nlst

compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) | x == y = compress (x:xs)
                  | otherwise = x : compress (y : xs)

testCompress = quickCheck $ \ lst -> let cmp = compress (lst :: [Int])
                                     in (length cmp) <= (length lst) && 
                                        all (\e -> elem e cmp) lst &&
                                        and (zipWith (/=) cmp (tail cmp))

pack :: Eq a => [a] -> [[a]]
pack lst = inner lst []
    where 
      inner [] [] = []
      inner [] l = [l]
      inner (x:xs) [] = inner xs [x]
      inner l1@(x:xs) l2@(y:ys) | x == y = inner xs (x:y:ys)
                                | otherwise = l2 : inner l1 []

testPack = quickCheck $ \ lst -> pack (lst :: [Char]) == group lst

encode :: Eq a => [a] -> [(Int, a)]
encode lst = map (\l -> (length l, head l)) $ pack lst

testEncode = quickCheck $ \lst -> let enc = encode (lst :: [Char])
                                  in (length enc <= length lst) &&
                                     (sum (map fst enc) == length lst) &&
                                     all (\e -> elem e $ map snd enc) lst

data RLEElem a = Single a
               | Multiple Int a
                 deriving Show

rleLen :: RLEElem a -> Int
rleLen (Single _) = 1
rleLen (Multiple n _) = n

rleElem :: RLEElem a -> a
rleElem (Single x) = x
rleElem (Multiple _ x) = x

encodeModified :: Eq a => [a] -> [RLEElem a]
encodeModified lst = map toRle $ encodeDirect lst
    where toRle (1, e) = Single e
          toRle (n, e) = Multiple n e

repeatN :: Int -> a -> [a]
repeatN 1 x = [x]
repeatN n x = x : repeatN (n - 1) x

decodeModified :: [RLEElem a] -> [a]
decodeModified [] = []
decodeModified (Single x : l) = x : decodeModified l
decodeModified (Multiple n x : l) = (repeatN n x) ++ decodeModified l

testEncodeModified = quickCheck $ \lst -> let enc = encodeModified (lst :: [Char])
                                          in (length enc <= length lst) &&
                                                 (sum (map rleLen enc) == length lst) &&
                                                 all (\e -> elem e $ map rleElem enc) lst &&
                                                 decodeModified enc == lst


encodeDirect :: Eq a => [a] -> [(Int, a)]
encodeDirect lst = inner lst Nothing
    where 
      inner [] Nothing = []
      inner [] (Just l) = [l]
      inner (x:xs) Nothing = inner xs $ Just (1, x)
      inner l1@(x:xs) (Just l@(n, el)) | x == el = inner xs $ Just (n + 1, el)
                                       | otherwise = l : inner l1 Nothing

testEncodeDirect = quickCheck $ \lst -> let enc = encodeDirect (lst :: [Char])
                                        in (length enc <= length lst) &&
                                               (sum (map fst enc) == length lst) &&
                                               all (\e -> elem e $ map snd enc) lst
dupli :: [a] -> [a]
dupli [] = []
dupli (x : xs) = x : x : dupli xs

testDupli = quickCheck $ \ lst -> let dup = dupli (lst :: [Int])
                                  in dup == concatMap (\ e -> [e,e]) lst


repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x : xs) n = repeatN n x ++ repli xs n

testRepli = quickCheck $ \ lst -> forAll (choose (1, 10)) $
            \ num -> let dup = repli (lst :: [Int]) num
                     in dup == concatMap (\ e -> repeatN num e) lst


dropEvery :: [a] -> Int -> [a]
dropEvery lst num = inner lst num num
    where inner [] _ _ = []
          inner (x:xs) 1 n = inner xs n n
          inner (x:xs) i n = x : inner xs (i - 1) n

testDropEvery = quickCheck $ \lst -> forAll (choose (1, 10)) $
                \ n -> let dropped = dropEvery (lst :: [Int]) n 
                       in (length dropped) == (length lst - (length lst) `div` n)

take1 :: Int -> [a] -> [a]
take1 _ [] = []
take1 0 lst = []
take1 n (x : xs) = x : take1 (n - 1) xs

drop1 :: Int -> [a] -> [a]
drop1 _ [] = []
drop1 0 lst = lst
drop1 n (x : xs) = drop1 (n - 1) xs

split :: [a] -> Int -> ([a], [a])
-- split lst n = inner n lst
--     where inner _ [] = ([], [])
--           inner 0 lst = ([], lst)
--           inner n (x:xs) = let r1 = inner (n - 1) xs in (x : fst r1, snd r1)
split lst n = (take1 n lst, drop1 n lst)

testSplit = quickCheck $ \ lst -> forAll (choose (1,10)) $
            \ n -> split (lst :: [Int]) n == splitAt n lst

slice :: [a] -> Int -> Int -> [a]
slice lst start end = inner lst start end
    where inner [] _ _ = []
          inner l 1 0 = []
          inner (x:xs) 1 n = x : inner xs 1 (n - 1)
          inner (x:xs) n n2 = inner xs (n - 1) (n2 - 1)

testSlice = quickCheck $ \lst -> forAll (choose (1, 10)) $
            \ n1 -> forAll (choose (0,10)) $
                    \ n2 -> slice (lst :: [Int]) n1 (n1 + n2) == (take (n2 + 1) $ drop (n1 - 1) lst)


rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate lst 0 = lst
rotate lst n | n > 0 = let normN = n `mod` length lst
                       in drop1 normN lst ++ take1 normN lst
rotate lst n | n < 0 = rotate lst (length lst + n)

testRotate = quickCheck $ \lst -> forAll (choose (-50, 50)) $
             \ n -> let rot = rotate (lst :: [Int]) n
                    in (null lst && null rot) ||
                       ((n `mod` (length lst)) == 0 && lst == rot) ||
                       (lst /= rot && lst == rotate rot (- n)) ||
                       (lst == rot && lst == rotate rot 1)

removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "removeAt: index is out of bounds"
removeAt 1 (x:xs) = (x, xs)
removeAt n (x:xs) = let ~(res, l) = removeAt (n - 1) xs
                    in (res, x:l)

testRemoveAt = quickCheck $ \(NonEmpty lst) -> forAll (choose (1, length lst)) $
               \ n -> let (res, newLst) = removeAt n (lst :: [Int])
                      in (newLst == take (n - 1) lst ++ drop n lst) &&
                         res == lst !! (n - 1)
