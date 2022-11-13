
--Alias para variables
type Nombre = String
--Tipo para las proposiciones
data LProp = T | F | VarP Nombre | Conj LProp LProp | Disy LProp LProp |
                Impl LProp LProp | Syss LProp LProp | Neg LProp deriving (Show, Eq)

-- -- make a show instance for LProp
-- instance Show LProp where
--     show T = "True"
--     show F = "False"
--     show (VarP x) = x
--     show (Neg x) = "!" ++ show x
--     show (Conj x y) = "(" ++ show x ++ " ∧ " ++ show y ++ ")"
--     show (Disy x y) = "(" ++ show x ++ " ∨ " ++ show y ++ ")"
--     show (Impl x y) = "(" ++ show x ++ " → " ++ show y ++ ")"
--     show (Syss x y) = "(" ++ show x ++ " ↔ " ++ show y ++ ")"


--Tipo para el Tableaux
data Tableaux = Hoja [LProp] | Alpha [LProp] Tableaux |
                 Beta [LProp] Tableaux Tableaux deriving (Show, Eq)




{- literales (1 punto) Función que nos dice si en una lista de fórmulas, todas son literales. -}
literales :: [LProp] -> Bool
literales [] = True
literales (x:xs) = case x of
  VarP _ -> literales xs
  Neg (VarP _) -> literales xs
  _ -> False

{- nextF (1 punto) Función que regresa la primera fórmula que no es literal de una lista de fórmulas. -}

nextF :: [LProp] -> LProp
nextF [] = error "No hay fórmulas"
nextF (x:xs) = case x of
  VarP _ -> nextF xs
  Neg (VarP _) -> nextF xs
  _ -> x

{- alpha (1 punto) Nos dice si una fórmula f es una fórmula α como tabla semántica. -}

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



{- sigma (1 punto) Nos dice si una fórmula f es una fórmula σ , las fórmulas σ ¬¬σ ¬True ¬False -}

sigma :: LProp -> Bool
sigma (Neg (Neg x)) = True
sigma (Neg T) = False
sigma (Neg F) = True
sigma _ = False

