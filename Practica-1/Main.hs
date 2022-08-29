-- DAVID RIVERA MORALES 320176876

-- La función par n, que recibe un entero y contesta si es o no un número par.
par :: Int ->  IO [Char]
par n = if (mod n 2) == 0 then do {return "Es par"} else do {return "No es par"}

-- La función impar n, que recibe un entero y contesta si es o no un número impar.
impar :: Int -> IO [Char]
impar n = if (mod n 2) /= 0 then do {return "Es impar"} else do {return "No es impar"}

-- La función minimo n m que recibe dos enteros y contesta con el menor.
minimo :: Int -> Int -> Int
minimo n m = if (n <= m) then  n else m

-- La función maximo n m que recibe dos enteros y contesta con el mayor.
maximo :: Int -> Int -> Int
maximo n m = if (n >= m) then  n else  m

-- La función absoluto n, que recibe un entero y regresa su valor absoluto.
absoluto :: Int -> Int
absoluto n = if n >= 0 then  n else -n

-- La función divE n m, que recibe dos enteros y devuelve la división entera de n y m.
divE :: Int -> Int -> Int
divE n m = if (n < m)
        then n 
        else divE (n-m) m 

-- Tu propia versión de head y tail que se llamen cabeza y cola respectivamente.
cabeza :: [Int] -> Int 
cabeza [x] = x
cabeza (x:xs) = x

cola :: [Int] -> [Int] 
cola [] = []
cola (x:xs) = xs

-- La función quita n lst, que recibe un número entero positivo n y una lista lst para devol-
-- ver lst pero sin sus primeros n elementos.
quita :: Int -> [Int] -> [Int]
quita 0 lst = lst
quita n (x:xs) = quita (n-1) xs 

-- La función enesimo n lst que toma un entero n y una lista lst para regresar el enésimo
-- elemento de lst. Los computologos siempre empezamos a contar desde el cero.
enesimo :: Int -> [Int] -> Int
enesimo 0 lst = cabeza lst
enesimo n (x:xs) = enesimo (n-1) xs








