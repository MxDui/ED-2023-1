data LProp
  = PTrue
  | PFalse
  | Var Nombre
  | Neg LProp
  | Conj LProp LProp
  | Disy LProp LProp
  | Impl LProp LProp
  | Syss LProp LProp

type Nombre = String -- Nombre es un sinonimo para String.

type Asignacion = [(Nombre, Int)] -- Una asignación es una lista de tuplas.

instance Show LProp where
  show PTrue = "True"
  show PFalse = "False"
  show (Var x) = x
  show (Neg x) = "!" ++ show x
  show (Conj x y) = "(" ++ show x ++ " ∧ " ++ show y ++ ")"
  show (Disy x y) = "(" ++ show x ++ " ∨ " ++ show y ++ ")"
  show (Impl x y) = "(" ++ show x ++ " → " ++ show y ++ ")"
  show (Syss x y) = "(" ++ show x ++ " ↔ " ++ show y ++ ")"

vars :: LProp -> [Nombre]
vars PTrue = []
vars PFalse = []
vars (Var x) = [x]
vars (Neg x) = vars x
vars (Conj x y) = vars x ++ vars y
vars (Disy x y) = vars x ++ vars y
vars (Impl x y) = vars x ++ vars y
vars (Syss x y) = vars x ++ vars y

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

conm :: LProp -> LProp
conm (Conj x y) = Conj y x
conm (Disy x y) = Disy y x
conm (Impl x y) = Impl y x
conm (Syss x y) = Syss y x
conm x = x

dist :: LProp -> LProp
dist (Conj x (Disy y z)) = Disy (Conj x y) (Conj x z)
dist (Conj (Disy y z) x) = Disy (Conj y x) (Conj z x)
dist (Disy x (Conj y z)) = Conj (Disy x y) (Disy x z)
dist (Disy (Conj y z) x) = Conj (Disy y x) (Disy z x)
dist (Impl x y) = Impl x y
dist (Syss x y) = Syss x y
dist x = x

deMorgan :: LProp -> LProp
deMorgan (Neg (Conj x y)) = Disy (Neg x) (Neg y)
deMorgan (Neg (Disy x y)) = Conj (Neg x) (Neg y)
deMorgan x = x

equiv_op :: LProp -> LProp
equiv_op (Conj x y) = Conj (equiv_op x) (equiv_op y)
equiv_op (Disy x y) = Disy (equiv_op x) (equiv_op y)
equiv_op (Impl x y) = Disy (Neg (equiv_op x)) (equiv_op y)
equiv_op (Syss x y) = Conj (Disy (Neg (equiv_op x)) (equiv_op y)) (Disy (Neg (equiv_op y)) (equiv_op x))
equiv_op x = x

dobleNeg :: LProp -> LProp
dobleNeg (Neg (Neg x)) = dobleNeg x
dobleNeg (Conj x y) = Conj (dobleNeg x) (dobleNeg y)
dobleNeg (Disy x y) = Disy (dobleNeg x) (dobleNeg y)
dobleNeg (Impl x y) = Impl (dobleNeg x) (dobleNeg y)
dobleNeg (Syss x y) = Syss (dobleNeg x) (dobleNeg y)
dobleNeg (Neg x) = Neg (dobleNeg x)
dobleNeg x = x

num_conectivos :: LProp -> Int
num_conectivos PTrue = 0
num_conectivos PFalse = 0
num_conectivos (Var x) = 0
num_conectivos (Neg x) = 1 + num_conectivos x
num_conectivos (Conj x y) = 1 + num_conectivos x + num_conectivos y
num_conectivos (Disy x y) = 1 + num_conectivos x + num_conectivos y
num_conectivos (Impl x y) = 1 + num_conectivos x + num_conectivos y
num_conectivos (Syss x y) = 1 + num_conectivos x + num_conectivos y

interpretacion :: LProp -> Asignacion -> Int
interpretacion PTrue asig = 1
interpretacion PFalse asig = 0
interpretacion (Var a) asig = asignaValor a asig
interpretacion (Neg expr) vs = (interpretacion expr vs) - 1
interpretacion (Conj exp1 exp2) vs = if (interpretacion exp1 vs) == 1 && (interpretacion exp2 vs) == 1 then 1 else 0
interpretacion (Disy exp1 exp2) vs = if (interpretacion exp1 vs) == 0 || (interpretacion exp2 vs) == 0 then 1 else 0 -- check this
interpretacion (Impl exp1 exp2) vs = if (interpretacion exp2 vs) == 1 || (interpretacion exp1 vs) - 1 == 0 then 0 else 1
interpretacion (Syss exp1 exp2) vs = if (interpretacion exp1 vs) == (interpretacion exp2 vs) then 1 else 0

-- Funcion que dada una asignación, coloca a cada letra el valor requerido. Auxiliar de interpretación
asignaValor :: Eq a => a -> [(a, b)] -> b
asignaValor x ((a, b) : xs) = if a == x then b else asignaValor x xs