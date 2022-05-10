concat_lists :: [Int] -> [Int] -> [Int]

concat_lists [] y = y
concat_lists x y = (head x) : concat_lists (tail x) y

main :: IO ()
main = do
let l1 = [3,4,5]
let l2 = [10,20,30]
let l3 = [8,88,888]
let res1 = concat_lists l1 (concat_lists l2 l3)
let res2 = concat_lists (concat_lists l1 l2) l3 
putStr "res1 = "
print res1
putStr "res2 = "
print res2
return()