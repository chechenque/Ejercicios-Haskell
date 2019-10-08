module EjerSem3 where

--Dado un entero n y una lista, nos regresa una lista sin los n elementos eliminados 
tira :: Int -> [a] -> [a]
tira _ [] = []
tira 0 (x:xs) = (x:xs) 
tira n (x:xs) = (tira (n-1) xs)

--Dadas dos tripletas
--Nos regresa una nueva tripleta
--La primera entrada será la suma de las dos primeras entradas anteriores, el resultado es String
--La segunda entrada será el producto de las dos segundas entradas aiores, el resultado es Int
opTrip :: (Int,String,String) -> (String,Int,String) -> (String,String,Int)
opTrip  (a,b,c) (d,e,f) = (show (a + (read d)), show ((read b) * e),((read c) + (read f)))
                                                        
--Dados dos enteros, n y m
--Nos regresa el resultado de calcular n a la m
potencia :: Int -> Int -> Int
potencia _ 0 = 1 
potencia n m = n * potencia n (m -1)

--Dada una lista de enteros
--Nos regresa una nueva lista pero solo con los números pares
listaPares :: [Int] -> [Int]
listaPares [] = []
listaPares (x:xs) = if (mod x 2 == 0)
                    then [x] ++ (listaPares xs) 
                    else listaPares xs




--Dado un entero n y una lista de enteros
--Nos regresa una lista sin la apariencia del entero dado
elimina :: Int -> [Int] -> [Int]
elimina  _ [] = []
elimina n (x:xs) = if( n /= x) 
                   then [x] ++ (elimina n xs)
                   else elimina n xs

--La sintaxis de un if es así
--en ambos casos cuando tienen que hacer recursion con listas
--tienen esto

--funcion:: [a] -> [a]
--funcion [] = []
--funcion (x:xs) = if (condicion)
	  	 --  then siEsTrueEjecutaEsto
		  -- else siEsFalseEjecutaEsto

--en la parte de condicion por lo general preguntan acerca de la cabeza
--y en el then y else es cuando ponen lo de guardar la cabeza en una lista
--algo como [x] ++ funcion xs
--esto es para guardar la cabeza en una lista y luego hacer la recursion en la cola


{-- Pruebas --}

--Debe regresar [3452,230230,1]
pruebaTira1 = tira 5 [2,6,24,67,100000,3452,230230,1]

--Debe regresar []
pruebaTira2 = tira 10 ["d","t","y"]

--Debe regresar ("59","-418",1)
pruebaOp1 = opTrip (34,"22","12") ("25",-19,"-11")

--Debe regresar ("-39","169",90)
pruebaOp2 = opTrip (-15,"13","56") ("-24",13,"34")

--Debe regresar 1296
pruebaPot1 = potencia 6 4

--Debe regresar 707281
pruebaPot2 = potencia 29 4

--Debe regresar [-12,0,-24,56]
pruebaPar1 = listaPares [3,23,-12,0,-24,1,56]

--Debe regresar [2,8,-132,156]
pruebaPar2 = listaPares [1,5,2,8,-3,-9,-132,156]

--Debe regresar [4,6,1,55,555,1000]
pruebaElim1 = elimina 5 [5,4,5,6,1,5,55,555,1000,5,5]

--Debe regresar [1,111,111,111,1111,1111,1]
pruebaElim2 = elimina 11 [1,11,111,111,111,11,11,11,1111,1111,11,11,1]

