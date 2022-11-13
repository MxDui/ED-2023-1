
--Alias para variables
type Nombre = String
--Tipo para las proposiciones
data LProp = T | F | VarP Nombre | Conj LProp LProp | Disy LProp LProp |
                Impl LProp LProp | Syss LProp LProp | Neg LProp deriving (Show, Eq)

--Tipo para el Tableaux
data Tableaux = Hoja [LProp] | Alpha [LProp] Tableaux |
                 Beta [LProp] Tableaux Tableaux deriving (Show, Eq)

instance Show LProp where
  show PTrue = "True"
  show PFalse = "False"
  show (Var x) = x
  show (Neg x) = "!" ++ show x
  show (Conj x y) = "(" ++ show x ++ " ∧ " ++ show y ++ ")"
  show (Disy x y) = "(" ++ show x ++ " ∨ " ++ show y ++ ")"
  show (Impl x y) = "(" ++ show x ++ " → " ++ show y ++ ")"
  show (Syss x y) = "(" ++ show x ++ " ↔ " ++ show y ++ ")"


{- literales (1 punto) Función que nos dice si en una lista de fórmulas, todas son literales. -}
