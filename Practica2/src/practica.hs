-- potencia b p, que recibe un número b y lo eleva a la p.
potencia:: Int -> Int -> Int
potencia b 0 = 1
potencia b p = b * potencia b (p-1)

-- suma_pares n, que recibe un número par n y suma todos los números pares anteriores a n, incluyendo
-- a n.
suma_pares :: Int -> Int
suma_pares 0 = 0
suma_pares n = if mod n 2 == 0 then n + suma_pares (n-1) else suma_pares (n-1)

-- Escribe la función triangular n, que recibe un entero regresa el n-ésimo número triangular, osease:
triangular :: Int -> Int
triangular 0 = 0
triangular n = n + triangular (n-1)

--fibo n, que recibe un entero n y devuelve el número correspondiente al lugar n de la sucesión de
--Fibonacci.
fibo :: Int -> Int
fibo 0 = 0
fibo n = if n == 1 then 1 else fibo (n-1) + fibo (n-2)

--ultimo lst, que recibe una lista y responde con el último elemento de la lista.
ultimo :: [a] -> a
ultimo [ ] = error "No hay elementos"
ultimo [x] = x
ultimo (x:xs) = ultimo xs

-- reversa lst, que recibe una lista y devuelve la misma lista al revés.
reversa :: [a] -> [a]
reversa [ ] = error "No hay elementos"
reversa [x] = [x]
reversa (x:xs) = reversa xs ++ [x]

--elemento elm lst, que recibe una lista lst de tipo [a] y elm un elemento de tipo a y responde si
--elm pertenece a lst.
elemento :: Eq a => a -> [a] -> Bool
elemento elm [ ] = False
elemento elm (x:xs) = if elm == x then True else elemento elm xs


data Categoria = Perfecto | Deficiente | Abundante

nicomaco :: Int -> Categoria
nicomaco 0 = Deficiente
nicomaco n = if suma_divisores n == n then Perfecto else if suma_divisores n < n then Deficiente else Abundante

suma_divisores :: Int -> Int
suma_divisores 0 = 0
suma_divisores n = if mod n 2 == 0 then n + suma_divisores (n-1) else suma_divisores (n-1)

luhn :: Num a => [a] -> [a]
luhn [] = error "No es válida"
luhn (x:xs) =   



 




