# Práctica 5

## David Rivera Morales -- 320176876

## Israel Rivera -- 320490747

### La función literales

```haskell
literales :: [LProp] -> Bool
literales [] = True
literales (x : xs) = case x of
  VarP _ -> literales xs
  Neg (VarP _) -> literales xs
  _ -> False
```

### La función nexF

```haskell
nextF :: [LProp] -> LProp
nextF [] = error "No hay fórmulas"
nextF (x : xs) = case x of
  VarP _ -> nextF xs
  Neg (VarP _) -> nextF xs
  _ -> x
```

### La función alpha

```haskell
alpha :: LProp -> Bool
alpha (Conj x y) = case x of
  Disy _ _ -> False
  Impl _ _ -> False
  _ -> case y of
    Disy _ _ -> False
    Impl _ _ -> False
    _ -> True
alpha (Neg (Disy x y)) = case x of
  Conj _ _ -> False
  Impl _ _ -> False
  _ -> case y of
    Conj _ _ -> False
    Impl _ _ -> False
    _ -> True
alpha (Neg (Impl x y)) = case x of
  Conj _ _ -> False
  Disy _ _ -> False
  _ -> case y of
    Conj _ _ -> False
    Disy _ _ -> False
    _ -> True
alpha _ = False
```

### La función beta

```haskell
beta :: LProp -> Bool
beta (Disy x y) = case x of
  Conj _ _ -> True
  Impl _ _ -> True
  _ -> case y of
    Conj _ _ -> True
    Impl _ _ -> True
    _ -> True
beta (Neg (Conj x y)) = case x of
  Disy _ _ -> True
  Impl _ _ -> True
  _ -> case y of
    Disy _ _ -> True
    Impl _ _ -> True
    _ -> True
beta (Neg (Impl x y)) = case x of
  Conj _ _ -> True
  Disy _ _ -> True
  _ -> case y of
    Conj _ _ -> True
    Disy _ _ -> True
    _ -> True
beta _ = False
```

### La función sigma

```haskell
sigma :: LProp -> Bool
sigma (Neg (Neg x)) = True
sigma (Neg T) = False
sigma (Neg F) = True
sigma _ = False
```

### La función expSigma

```haskell
expSigma :: [LProp] -> LProp -> [LProp]
expSigma [] _ = []
expSigma (x : xs) f = case f of
  Neg (Neg x) -> [x] ++ xs
  Neg T -> [F] ++ xs
  Neg F -> [T] ++ xs
  _ -> [f] ++ xs
```

### La función expAlpha

```haskell
expAlpha :: [LProp] -> LProp -> [LProp]
expAlpha [] _ = []
expAlpha (x : xs) f = case f of
  Conj x y -> [x] ++ [y] ++ xs
  Neg (Disy x y) -> [Neg x] ++ [Neg y] ++ xs
  Neg (Impl x y) -> [x] ++ [Neg y] ++ xs
  _ -> [f] ++ xs
```

### La función expBeta

```haskell
extract :: [LProp] -> LProp -> [LProp]
extract [] _ = []
extract (x : xs) f = case x of
  VarP _ -> [x] ++ extract xs f
  Neg (VarP _) -> [x] ++ extract xs f
  _ -> extract xs f

expBeta :: [LProp] -> LProp -> ([LProp], [LProp])
expBeta [] _ = ([], [])
expBeta [x] _ = ([x], [])
expBeta (x : xs) f =
  let first = [x]
   in case f of
        Disy x y -> ([x] ++ extract xs f ++ first, [y] ++ extract xs f ++ first)
        Neg (Conj x y) -> ([Neg x] ++ extract xs f ++ first, [Neg y] ++ extract xs f ++ first)
        Impl x y -> ([Neg x] ++ extract xs f ++ first, [y] ++ extract xs f ++ first)
        _ -> ([f] ++ extract xs f ++ first, [f] ++ extract xs f ++ first)
```

### La función consTableaux

```haskell
consTableaux :: LProp -> Tableaux
consTableaux f = case f of
  VarP _ -> Hoja [f]
  Neg (VarP _) -> Hoja [f]
  _ -> case f of
    Conj x y -> Alpha [f] (consTableaux x)
    Disy x y -> Beta [f] (consTableaux x) (consTableaux y)
    Neg (Disy x y) -> Alpha [f] (consTableaux (Neg x))
    Neg (Conj x y) -> Beta [f] (consTableaux (Neg x)) (consTableaux (Neg y))
    Impl x y -> Beta [f] (consTableaux (Neg x)) (consTableaux y)
    Neg (Impl x y) -> Alpha [f] (consTableaux x)
    Syss x y -> Alpha [f] (consTableaux (Impl x y))
    Neg (Syss x y) -> Beta [f] (consTableaux (Impl x y)) (consTableaux (Impl y x))
    Neg (Neg x) -> consTableaux x
    _ -> Hoja [f]
```
