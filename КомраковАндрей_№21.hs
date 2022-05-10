-- Функция перевода вещественного числа с заданной точностью
num_to_real :: Float -> Int -> Int
num_to_real k n = round(2^n*k)

-- Функция рекурсивного посимвольного сравнения с заданной точностью 
separate :: Int -> Float -> Float -> Int
separate n x y = separ 0 x y
 where separ i x y = if i == n then 0 else if abs d > 1 then d else separ (i+1) x y
        where d = num_to_real x i - num_to_real y i

-- Функции для сравнения с заданной точностью, возвращающие bool 
-- gt - greater then 
real_gt n x y = separate n x y > 0
-- ge - greater or equal 
real_ge n x y = separate n x y >= 0
-- lt - less then
real_lt n x y = separate n x y < 0
-- le - less or equal 
real_le n x y = separate n x y <= 0
-- e - equal 
real_e n x y = separate n x y == 0