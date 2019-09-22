module SuperDigito where
digito :: Int -> Int
digito n = if n<10
         then n
         else digito(superSuma n)
       
superSuma :: Int -> Int
superSuma 0 = 0
superSuma n = rem n 10 + superSuma(div n 10)