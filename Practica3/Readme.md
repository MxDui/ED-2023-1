# Práctica 3

## David Rivera Morales -- 320176876

## Israel Rivera --

---

### La función longP

Dentro de esta función el caso base es una lista vacía que da 0 ya que una lista sin elementos , no tiene longitud por lo que es 0. En el caso de la recursión lo que hice fue que si la lista no es vacía entonces se le suma 2 a la longitud de la lista que se obtiene al quitar la primer sublista de la lista original. Esto se hace ya que la lista que se obtiene al quitar la primer sublista es una lista que tiene una longitud menor que la lista original y por lo tanto se le suma 2 a la longitud de la lista que se obtiene al quitar la primer sublista.

```haskell
longP :: EList a -> Int
longP [] = 0
longP ((x,y):xs) = 2 + longP xs
```

### La función elemP

Dentro de esta función el caso base es una lista vacía que da False ya que una lista sin elementos , no tiene elementos por lo que es False. En la recursión lo que hice fue comparar el elemento tanto con el primer valor de la sublista como con el segundo valor de la sublista y si alguno de los dos es igual al elemento que se busca entonces se regresa True, de lo contrario se llama a la función con la cola de la lista.

```haskell
elemP :: Eq a => a -> EList a -> Bool
elemP e [] = False
elemP e ((x,y):xs) = e == x || e == y || elemP e xs
```
