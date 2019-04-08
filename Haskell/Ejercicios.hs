--1. Determina el resultado de un número x elevado a una potencia n 
potencia :: Int -> Int -> Int
potencia x n = x^n

--2. Determina si un número n se encuentra en un rango determinado
rango a b c = if(c>=a && c<=b) then "esta dentro del rango" else "no est dentro"

--3. Dado un número entero en segundos, determinar la cantidad de horas, minutos y segundos que contiene. 
segundosAHora :: Integer -> (Integer, Integer,Integer,Integer)
segundosAHora s = (dias, horas, minutos, segundos) where  
 dias = div s 86400
 dd = mod s 86400
 horas = div dd 3600
 ss = mod s 3600
 minutos = div ss 60
 segundos =  mod ss 60

--4. Determine el mayor de 4 enteros 
ordenacion :: Integer -> Integer -> Integer
ordenacion x y = div ((x + y) + abs(x - y)) 2

mayorde4 :: Integer -> Integer -> Integer -> Integer -> Integer
mayorde4 w x y z = ordenacion (ordenacion w x) (ordenacion y z)

--5. Calcula la suma de una lista (arreglo) de elementos. 
SumaLista numeros=foldl1 (+) xs

--6. Determina si un elemento dado está contenido en una lista. Devuelve verdadero o falso.
buscarLista::Int ->Bool
buscarLista a = a `elem` [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]

--7. Determina si dada una lista, ésta se encuentra ordenada. Se debe devolver verdadero o falso.
estaOrdenada :: Ord a => [a] -> Bool
estaOrdenada [] = True
estaOrdenada [x] = True
estaOrdenada (x:y:zs)
 |x <= y = estaOrdenada (y:zs)
 |otherwise = False

--8. Dadas dos listas, determine si son iguales. Devolver verdadeo o falso. (9 pts)
sonIgual :: Eq a => [a] -> [a] -> Bool
sonIgual (_:xs) [] = False
sonIgual [] _ = True
sonIgual (x:xs) (y:ys) = (x == y) && sonIgual xs ys

--9. Realizar un función recursiva que retorne como salida el resultado de la suma 1 + 3 + 5+N
sumaNumeros:: Int -> Int
sumaNumeros 0=0
sumaNumeros n= n+sumaNumeros(n-2)

--11. Realiza una función en Haskell que permita cargar calcular la unión, intersección y
diferencia de dos conjuntos datos. Para esto puede hacer uso de la librería “Data.set”