-- x ++ y -- 
append :: [Int] -> [Int] -> [Int]
append [] y = y
append x y = (head x) : append (tail x) y

-- reverse x -- 
rev :: [Int] -> [Int]
rev [] = []
rev x = append (rev (tail x)) [head x]

main  :: IO()
main = do

let l1 = [1,2,3]
let l2 = [4,5,6]

let res1 = rev (append l1 l2) 
let res2 = append (rev l2) (rev l1)

putStr("append l1 l2")
print(append l1 l2)
putStr("reverse l1")
print(rev l1)
putStr("\nres1=")
print(res1)
putStr("res2=")
print(res2)

return()
