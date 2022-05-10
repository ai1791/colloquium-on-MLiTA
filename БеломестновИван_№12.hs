import Data.IORef
import Data.Array
import Data.Array.IO

--Создание неизменяемых массивов
a = array (1, 4) [(1, 2), (2, 3), (3, 10), (4, 25)]
a1 = a//[(4, 6)]
a2 = array (1, 4) [(i, 2^i) | i <- [1..4]]

main = do
--Использование ссылок:
  putStrLn "References:"
  var <- newIORef 120
  x1 <- readIORef var
  print x1
  readIORef var >>= print
  writeIORef var 23
  x2 <- readIORef var
  print x2
  readIORef var >>= print
--Неизменяемые массивы:
  putStrLn "Immutable Arrays:"
  print a
  print (a ! 2)
  print a1
  print a2
--Изменяемые массивы:
  putStrLn "Mutable Arrays:" 
  arr <- newArray (1,10) 37 :: IO (IOArray Int Int)
  a <- readArray arr 1
  writeArray arr 1 64
  b <- readArray arr 1
  print (a,b)
