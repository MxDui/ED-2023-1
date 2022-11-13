
--Alias para variables
type Nombre = String
--Tipo para las proposiciones
data LProp = T | F | VarP Nombre | Conj LProp LProp | Disy LProp LProp |
                Impl LProp LProp | Syss LProp LProp | Neg LProp deriving (Show, Eq)

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

{- beta (1 punto) Nos dice si una fórmula f es una fórmula β -}

{- sigma (1 punto) Nos dice si una fórmula f es una fórmula σ , las fórmulas σ ¬¬σ ¬True ¬False -}

sigma :: LProp -> Bool
sigma (Neg (Neg x)) = True
sigma (Neg T) = False
sigma (Neg F) = True
sigma _ = False

{- expSigma (1 punto) Dada una lista de fórmulas l y una fórmula f, realiza la expansión sigma de f sobre la lista l. -}
-- base case

expSigma :: [LProp] -> LProp -> [LProp]
expSigma [] _ = []
expSigma (x:xs) f = case x of
    Neg (Neg y) -> (y) : expSigma xs f
    Neg T -> (F) : expSigma xs f
    Neg F -> (T) : expSigma xs f
    _ -> x : expSigma xs f

{- expAlpha (1 punto) Dada una lista de fórmulas l y una fórmula f, realiza la expansión alpha de f sobre la lista l. -}

expAlpha :: [LProp] -> LProp -> [LProp]
expAlpha [] _ = []
expAlpha (x:xs) f = case x of
    Conj y z -> (y) : (z) : expAlpha xs f
    Neg (Disy y z) -> (Neg y) : (Neg z) : expAlpha xs f
    Neg (Impl y z) -> (y) : (Neg z) : expAlpha xs f
    _ -> x : expAlpha xs f

{- expBeta (1 punto) Dada una lista de fórmulas l y una fórmula f, realiza la expansión beta de f sobre la lista l. -}
