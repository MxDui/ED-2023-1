type EList a = [(a,a)]

-- longP Dada una EList obtiene su número de elementos.
longP :: EList a -> Int
longP [] = 0
longP ((x,y):xs) = 2 + longP xs

-- elemP Dado elemento e y una Elist l, verifica si e pertenece a l.

elemP :: Eq a => a -> EList a -> Bool
elemP e [] = False
elemP e ((x,y):xs) = e == x || e == y || elemP e xs

-- consP Dados dos elementos y una EList, los agrega al principio de la Elist.

consP :: a -> a -> EList a -> EList a
const e1 e2 [] = [(e1,e2)]
consP e1 e2 ((x,y):xs) = (e1,e2):(x,y):xs

-- appendP Función que realiza la concatenación de dos EList.

appendP :: EList a -> EList a -> EList a
appendP [] l = l
appendP ((x,y):xs) l = (x,y):appendP xs l

-- snocP Función que agrega dos elementos al final de una EList.
snocP :: (a,a) -> EList a -> EList a
snocP (e1,e2) [] = [(e1,e2)]
snocP (e1,e2) ((x,y):xs) = (x,y):snocP (e1,e2) xs

-- atP Dada una EList l y un entero n, regresa el n-ésimo elemento de l.
atP :: EList a -> Int -> a
atP [] n = error "No existe el elemento"
atP ((x,y):xs) 1 = x
atP ((x,y):xs) 2 = y
atP ((x,y):xs) n = atP xs (n-2)

-- updateP Dada una EList l, un número n y un elemento e, cambia el n-ésimo elemento de l por e.