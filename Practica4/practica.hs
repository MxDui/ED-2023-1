data LProp = PTrue | PFalse | Var Nombre | Neg LProp | 
            Conj LProp LProp | Disy LProp LProp | Impl LProp LProp | 
            Syss LProp LProp

type Nombre = String -- Nombre es un sinonimo para String.
type Asignacion = [(Nombre, Int)] -- Una asignación es una lista de tuplas.

-- create an instance of the Show class for LProp

instance Show LProp where
  show PTrue = "True"
  show PFalse = "False"
  show (Var x) = x
  show (Neg x) = "¬" ++ show x
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
asocia_der (Impl x (Impl y z)) = Impl  (Impl y z) x
asocia_der (Syss x (Syss y z)) = Syss  (Syss y z) x
asocia_der x = x

