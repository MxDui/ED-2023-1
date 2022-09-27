type EList a = [(a, a)]

-- longP Dada una EList obtiene su número de elementos.
longP :: EList a -> Int
longP [] = 0
longP ((x, y) : xs) = 2 + longP xs

-- elemP Dado elemento e y una Elist l, verifica si e pertenece a l.

elemP :: Eq a => a -> EList a -> Bool
elemP e [] = False
elemP e ((x, y) : xs) = e == x || e == y || elemP e xs

-- consP Dados dos elementos y una EList, los agrega al principio de la Elist.

consP :: a -> a -> EList a -> EList a

const e1 e2 [] = [(e1, e2)]

consP e1 e2 ((x, y) : xs) = (e1, e2) : (x, y) : xs

-- appendP Función que realiza la concatenación de dos EList.

appendP :: EList a -> EList a -> EList a
appendP [] l = l
appendP ((x, y) : xs) l = (x, y) : appendP xs l

-- snocP Función que agrega dos elementos al final de una EList.
snocP :: (a, a) -> EList a -> EList a
snocP (e1, e2) [] = [(e1, e2)]
snocP (e1, e2) ((x, y) : xs) = (x, y) : snocP (e1, e2) xs

-- atP Dada una EList l y un entero n, regresa el n-ésimo elemento de l.
atP :: EList a -> Int -> a
atP [] n = error "No existe el elemento"
atP ((x, y) : xs) 1 = x
atP ((x, y) : xs) 2 = y
atP ((x, y) : xs) n = atP xs (n - 2)

-- updateP Dada una EList l, un número n y un elemento e, cambia el n-ésimo elemento de l por e.
updateP :: EList a -> Int -> a -> EList a
updateP [] n e = error "No existe el elemento"
updateP ((x, y) : xs) 1 e = (e, y) : xs
updateP ((x, y) : xs) 2 e = (x, e) : xs
updateP ((x, y) : xs) n e = (x, y) : updateP xs (n - 2) e

{-
aplanaP Función que recibe una EList l y regresa la misma pero aplanada, es decir, todos los elementos de l
separados en una lista común.
 -}
aplanaP :: EList a -> [a]
aplanaP [] = []
aplanaP ((x, y) : xs) = x : y : aplanaP xs

{- toEL Función que convierte una lista común en una EList, si la lista común es de longitud impar, no se
agregará el último elemento. -}

toEl :: [a] -> EList a
toEl [] = []
toEl [x] = []
toEl (x : y : xs) = (x, y) : toEl xs

{-dropP Dado un entero n y una EList l, borra los n primeros elementos de l, donde n es par. -}

dropP :: Int -> EList a -> EList a
dropP 0 l = l
dropP n [] = []
dropP n ((x, y) : xs) = if (mod n 2 == 0) then dropP (n - 2) xs else error "No se puede borrar un número impar de elementos"

{-
dropN Al igual que el inciso anterior, borra los n primeros elementos de l, pero n puede ser par o impar, pues
se regresará una lista común. -}

dropN :: Int -> EList a -> [a]

drop n [] = []

dropN 0 l = aplanaP l
dropN 1 ((x, y) : xs) = y : aplanaP xs
dropN n ((x, y) : xs) = dropN (n - 2) xs

{-
takeP Dado un entero n y una EList l, toma los n primeros elementos l, n debe ser par. -}
takeP :: Int -> EList a -> EList a
takeP 0 l = []
takeP n [] = []
takeP n ((x, y) : xs) = if (mod n 2 == 0) then (x, y) : takeP (n - 2) xs else error "No se puede tomar un número impar de elementos"

{-
takeN Al igual que el inciso anterior, toma los primeros n elementos de una EList, pero n puede ser par o impar,
pues los elementos se regresarán en una lista común. -}

takeN :: Int -> EList a -> [a]
takeN n [] = []
takeN 0 l = []
takeN 1 ((x, y) : xs) = [x]
takeN n ((x, y) : xs) = x : y : takeN (n - 2) xs

{- reversaP Elabora una función que haga la reversa de una EList. -}

reversaP :: EList a -> EList a
reversaP [] = []
reversaP ((x, y) : xs) = snocP (y, x) (reversaP xs)
