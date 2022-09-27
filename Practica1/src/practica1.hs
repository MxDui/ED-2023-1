-- 1)  La función par n, que recibe un entero y contesta si es o no un número par.
par :: Int -> Bool
par n = if (myMod n 2) == 0 then True else False

myMod :: Int -> Int -> Int
myMod n m = if (n < m)
then n 
else myMod (n-m) m
-- La función impar n, que recibe un entero y contesta si es o no un número impar.
--impar :: Int -> Bool
--impar m = (myMod n 2) == 1 then "es impar" else "no es impar"

-- 2) La función minimo n m que recibe dos enteros y contesta con el menor.

minimo :: Int -> Int -> Int
minimo n m = if (n < m) then n else m 

-- La función maximo n m que recibe dos enteros y contesta con el mayor.
maximo :: Int -> Int -> Int
maximo n m = if (n > m) then n else m


-- 3) (1 punto.) La función absoluto n, que recibe un entero y regresa su valor absoluto.
absoluto :: Int -> Int
absoluto n = if (n >= 0) then n else -n




-- 4) (2 puntos.) La función divE n m, que recibe dos enteros y devuelve la división entera de n y m.

divE :: Int -> Int -> Int 
divE n 0 = error "No"
divE n m = divEAux n m 0
divEAux :: Int -> Int -> Int -> Int
divEAux n m acc = if n < m 
then acc
else divEAux (n-m) m (acc+1)


-- 5) (1 punto.) Tu propia versión de head y tail que se llamen cabeza y cola respectivamente.



-- 6) (2 puntos.) La función quita n lst, que recibe un número entero positivo n y una lista lst para 
-- devolver lst pero sin sus primeros n elementos.
-- quita :: Int -> [lst] -> [lst]
-- quita n (x:xs) = quita n-1 xs  




-- 7) La función enesimo n lst que toma un entero n y una lista lst para regresar el enésimo
-- elemento de lst. Los computologos siempre empezamos a contar desde el cero.
