degree n m = 
  if m == 0 then 1 
  else n * degree n (m - 1)

main = print(degree 2 8)