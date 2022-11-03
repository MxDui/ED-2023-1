# Práctica 4

## David Rivera Morales -- 320176876

## Israel Rivera -- 320490747

### La función asocia_der

<!-- explicacion aqui -->

```haskell
asocia_der :: LProp -> LProp
asocia_der (Conj (Conj x y) z) = Conj x (Conj y z)
asocia_der (Conj x (Conj y z)) = Conj x (Conj y z)
asocia_der (Disy (Disy x y) z) = Disy x (Disy y z)
asocia_der (Disy x (Disy y z)) = Disy x (Disy y z)
asocia_der (Impl (Impl x y) z) = Impl x (Impl y z)
asocia_der (Impl x (Impl y z)) = Impl x (Impl y z)
asocia_der (Syss (Syss x y) z) = Syss x (Syss y z)
asocia_der (Syss x (Syss y z)) = Syss x (Syss y z)
asocia_der x = x
```

### La función asocia_izq

<!-- explicacion aqui -->

```haskell
asocia_izq :: LProp -> LProp
asocia_izq (Conj (Conj x y) z) = Conj (Conj x y) z
asocia_izq (Conj x (Conj y z)) = Conj (Conj x y) z
asocia_izq (Disy (Disy x y) z) = Disy (Disy x y) z
asocia_izq (Disy x (Disy y z)) = Disy (Disy x y) z
asocia_izq (Impl (Impl x y) z) = Impl (Impl x y) z
asocia_izq (Impl x (Impl y z)) = Impl (Impl x y) z
asocia_izq (Syss (Syss x y) z) = Syss (Syss x y) z
asocia_izq (Syss x (Syss y z)) = Syss (Syss x y) z
asocia_izq x = x
```

### La función conm

<!-- La explicacion aqui -->

```haskell
conm :: LProp -> LProp
conm (Conj x y) = Conj y x
conm (Disy x y) = Disy y x
conm (Impl x y) = Impl y x
conm (Syss x y) = Syss y x
conm x = x
```

### La función dist

<!-- explicacion aqui -->

```haskell
dist :: LProp -> LProp
dist (Conj x (Disy y z)) = Disy (Conj x y) (Conj x z)
dist (Conj (Disy y z) x) = Disy (Conj y x) (Conj z x)
dist (Disy x (Conj y z)) = Conj (Disy x y) (Disy x z)
dist (Disy (Conj y z) x) = Conj (Disy y x) (Disy z x )
dist (Impl x y) = Impl x y
dist (Syss x y) = Syss x y
dist x = x
```

### La función deMorgan

<!-- la explicacion aqui -->

```haskell
deMorgan :: LProp -> LProp
deMorgan (Neg (Conj x y)) = Disy (Neg x) (Neg y)
deMorgan (Neg (Disy x y)) = Conj (Neg x) (Neg y)
deMorgan x = x
```

### La función equiv_op

<!-- explicacion aqui -->

```haskell
equiv_op :: LProp -> LProp
equiv_op (Conj x y) = Conj (equiv_op x) (equiv_op y)
equiv_op (Disy x y) = Disy (equiv_op x) (equiv_op y)
equiv_op (Impl x y) = Disy (Neg (equiv_op x)) (equiv_op y)
equiv_op (Syss x y) = Conj (Disy (Neg (equiv_op x)) (equiv_op y)) (Disy (Neg (equiv_op y)) (equiv_op x))
equiv_op x = x
```

### La función dobleNeg

<!-- explicacion aqui -->

```haskell
dobleNeg :: LProp -> LProp
dobleNeg (Neg (Neg x)) = dobleNeg x
dobleNeg (Conj x y) = Conj (dobleNeg x) (dobleNeg y)
dobleNeg (Disy x y) = Disy (dobleNeg x) (dobleNeg y)
dobleNeg (Impl x y) = Impl (dobleNeg x) (dobleNeg y)
dobleNeg (Syss x y) = Syss (dobleNeg x) (dobleNeg y)
dobleNeg (Neg x) = Neg (dobleNeg x)
dobleNeg x = x
```

### La función num_conectivos

<!-- explicacion aqui -->

```haskell
num_conectivos :: LProp -> Int
num_conectivos PTrue = 0
num_conectivos PFalse = 0
num_conectivos (Var x) = 0
num_conectivos (Neg x) = 1 + num_conectivos x
num_conectivos (Conj x y) = 1 + num_conectivos x + num_conectivos y
num_conectivos (Disy x y) = 1 + num_conectivos x + num_conectivos y
num_conectivos (Impl x y) = 1 + num_conectivos x + num_conectivos y
num_conectivos (Syss x y) = 1 + num_conectivos x + num_conectivos y
```

### La función interpretacion

<!-- explicacion aqui -->

```haskell
interpretacion:: LProp -> Asignacion -> Int
interpretacion PTrue asig = 1
interpretacion PFalse asig = 0
interpretacion (Var a) asig = asignaValor a asig
interpretacion (Neg    expr)  vs = (interpretacion expr vs)-1
interpretacion (Conj   exp1 exp2) vs = if (interpretacion exp1 vs) == 1 && (interpretacion exp2 vs) == 1 then 1 else 0
interpretacion (Disy   exp1 exp2) vs = if (interpretacion exp1 vs) == 0 || (interpretacion exp2 vs) == 0 then 1 else 0 --check this
interpretacion (Impl   exp1 exp2) vs = if (interpretacion exp2 vs) == 1 || (interpretacion exp1 vs)-1 == 0 then 0 else 1
interpretacion (Syss exp1 exp2) vs = if (interpretacion exp1 vs) ==  (interpretacion exp2 vs) then 1 else 0

--Funcion que dada una asignación, coloca a cada letra el valor requerido. Auxiliar de interpretación
asignaValor:: Eq a => a -> [(a,b)] -> b
asignaValor x ((a,b):xs) = if a == x then b else asignaValor x xs
```
