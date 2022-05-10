
module Main where
---------------------------------------------------------------
import Numeric

showFullPrecision :: Double -> String
showFullPrecision x = showFFloat Nothing x ""

real_of_int :: Double -> Int -> Double
real_of_int a b = fromIntegral(round((2^(b))* (a)))

ndiv :: Double -> Double -> Int
ndiv x y = 
  let d = (x / y) in
  fromIntegral(round(d))
---------------------------------------------------------------
real_mul :: Double -> Double -> Int -> Double
real_mul x y n = 
     
      let r = floor(fromIntegral(n + 2) / 2) in
      let s = n+2 - r in
      let xr = real_of_int x r in
      let ys = real_of_int y s in
      let p = (ceiling(logBase 2 xr)) in
      let q = (ceiling(logBase 2 ys)) in
      let k = q+r+1 in
      let l = p+s+1 in
      let m = p+q+4+n in
      if (p == 0) && (q == 0) then 0
      else ((((real_of_int x k) * (real_of_int y l)) / (2^m)))


main = do
  print "result"

  print(showFullPrecision(real_mul 2.0 3.0 1))

  
