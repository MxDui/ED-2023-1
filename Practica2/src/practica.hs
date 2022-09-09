-- potencia b p, que recibe un número b y lo eleva a la p.

potencia:: Int -> Int -> Int
potencia b 0 = 1
potencia b p = b * potencia b (p-1)

-- suma_pares n, que recibe un número par n y suma todos los números pares anteriores a n, incluyendo
-- a n.

suma_pares :: Int -> Int
suma_pares 0 = 0
suma_pares n = if mod n 2 == 0 then n + suma_pares (n-1) else suma_pares (n-1)


--ultimo lst, que recibe una lista y responde con el último elemento de la lista.
ultimo :: [a] -> a
ultimo [ ] = error "No hay elementos"
ultimo [x] = x
ultimo (x:xs) = ultimo xs