module Test where

import Prelude hiding (sum,prod,append,appendl,map,concat,concatMap,length,sumLength,filter,sum,insert,unionSet,setOf,intersection)

sum :: [Int] -> Int
sum      = foldr (+) 0

prod :: [Int] -> Int
prod      = foldr (*) 1

map :: (a -> b) -> [a] -> [b]
map f = foldr ((:).f) []

concat :: [[a]] -> [a]
concat []     = []
concat (xs:xss) = xs ++ concat xss


concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f xs = concat (map f xs)


filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x xs -> if p x then x:xs else xs) []


length           :: [a] -> Int
length = foldr oneplus 0
         where oneplus x n = 1 + n

sumLength :: [[a]] -> Int
sumLength [] = 0
sumLength (x:xs) = length x + (sumLength xs)

append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x z@(y : ys)
  | x <= y = x : z
  | otherwise = y : insert x ys


setOf :: Eq a => [a] -> [a]
setOf [] = []
setOf (c:cs) = c : (setOf (filter (\ q -> q /= c) cs))


unionSet :: Eq a => [a] -> [a] -> [a]
unionSet s1 s2 = s1 ++ (filter (\ x -> not (x `elem` s1)) s2)






