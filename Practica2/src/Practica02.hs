{- | 
      Práctica 02
  Yael Antonio Calzada Martin
-}

-- Lenguaje de expresiones aritméticas y booleanas.
type Id = String

data EAB = V Id 
          | N Int 
          | B Bool
          | Succ EAB 
          | Pred EAB
          | Add EAB EAB 
          | Mul EAB EAB
          | Not EAB 
          | And EAB EAB 
          | Or EAB EAB
          | Lt EAB EAB 
          | Gt EAB EAB 
          | Eq EAB EAB
          | If EAB EAB EAB
          | Let Id EAB EAB
          deriving (Eq)

-- Instancia de la clase Show para el tipo de dato EAB.
instance Show EAB where
  show (V x) = x
  show (N n) = show n
  show (B b) = show b
  show (Succ n) = "Succ" ++ show n
  show (Pred n) = "Pred" ++ show n
  show (Add x y) = "(" ++ (show x) ++ " + " ++ (show y) ++ ")"
  show (Mul x y) = "(" ++ (show x) ++ " * " ++ (show y) ++ ")"
  show (Not n) = "Not" ++ show n
  show (And x y) = "(" ++ (show x) ++ " ^ " ++ (show y) ++ ")"
  show (Or x y) = "(" ++ (show x) ++ " v " ++ (show y) ++ ")"
  show (Lt x y) = "(" ++ (show x) ++ " < " ++ (show y) ++ ")"
  show (Gt x y) = "(" ++ (show x) ++ " > " ++ (show y) ++ ")"
  show (Eq x y) = "(" ++ (show x) ++ " = " ++ (show y) ++ ")"
  show (If x y z) = "If" ++ (show x) ++ " then " ++ (show y) ++ "else" ++ (show z)
  show (Let id x y) = "Let " ++ id ++ " = " ++ (show x) ++ " in " ++ (show y)

-- 1. Función freeVars que dada una expresión E de tipo EAB devuelve el conjunto de variables libres que aparecen en E.
freeVars :: EAB -> [Id]
freeVars e = toSet $ freeVarsAux e e []

freeVarsAux :: EAB -> EAB -> [Id] -> [Id]
freeVarsAux e (V id) l = if buscaVarLet e id then elimina id l else (id:l)
freeVarsAux e (N _) l = l
freeVarsAux e (B _) l = l
freeVarsAux e (Succ e1) l = freeVarsAux e e1 l
freeVarsAux e (Pred e1) l = freeVarsAux e e1 l
freeVarsAux e (Add e1 e2) l = (freeVarsAux e e2 l) ++ (freeVarsAux e e1 l)
freeVarsAux e (Mul e1 e2) l = (freeVarsAux e e2 l) ++ (freeVarsAux e e1 l)
freeVarsAux e (Not e1) l = freeVarsAux e e1 l  
freeVarsAux e (And e1 e2) l = (freeVarsAux e e2 l) ++ (freeVarsAux e e1 l)
freeVarsAux e (Or e1 e2) l = (freeVarsAux e e2 l) ++ (freeVarsAux e e1 l)
freeVarsAux e (Lt e1 e2) l = (freeVarsAux e e2 l) ++ (freeVarsAux e e1 l)                                                                                           
freeVarsAux e (Gt e1 e2) l = (freeVarsAux e e2 l) ++ (freeVarsAux e e1 l)   
freeVarsAux e (Eq e1 e2) l = (freeVarsAux e e2 l) ++ (freeVarsAux e e1 l)   
freeVarsAux e (If e1 e2 e3) l = (freeVarsAux e e3 l) ++ (freeVarsAux e e2 l) ++ (freeVarsAux e e1 l)
freeVarsAux e (Let id e1 e2) l = (freeVarsAux e e1 l) ++ (freeVarsAux e e2 l)

-- 2. Función substitution tal que substitution e x a es el resultado de reemplazar cada ocurrencia de x en e por a.
{- Vamos a definir un tipo Substitution -}
type Substitution = (Id , EAB)

substitution :: EAB -> Substitution -> EAB
substitution e (id, e1)
  | not $ pertenece id (freeVars e) = e
  | otherwise = substitutionAux e (id, e1)

substitutionAux :: EAB -> Substitution -> EAB
substitutionAux (V x) (id, e1) = if x == id then e1 else (V x)
substitutionAux (N n) t = N n
substitutionAux (B n) t = B n
substitutionAux (Succ e) t = Succ (substitutionAux e t)
substitutionAux (Pred e) t = Pred (substitutionAux e t)
substitutionAux (Add e1 e2) t = Add (substitutionAux e1 t) (substitutionAux e2 t)
substitutionAux (Mul e1 e2) t = Mul (substitutionAux e1 t) (substitutionAux e2 t)
substitutionAux (Not e1) t = Not (substitutionAux e1 t)
substitutionAux (And e1 e2) t = And (substitutionAux e1 t) (substitutionAux e2 t)
substitutionAux (Or e1 e2) t = Or (substitutionAux e1 t) (substitutionAux e2 t)
substitutionAux (Lt e1 e2) t = Lt (substitutionAux e1 t) (substitutionAux e2 t)
substitutionAux (Gt e1 e2) t = Gt (substitutionAux e1 t) (substitutionAux e2 t)
substitutionAux (Eq e1 e2) t = Eq (substitutionAux e1 t) (substitutionAux e2 t)
substitutionAux (If e1 e2 e3) t = If (substitutionAux e1 t) (substitutionAux e2 t) (substitutionAux e3 t)
substitutionAux (Let id e1 e2) t = Let id (substitutionAux e1 t) (substitutionAux e2 t)

-- 3. Función alphaEq que recibe dos expresiones y determina si son α-equivalentes.
{- Vamos a definir un tipo Substitution -}
alphaEq :: EAB -> EAB -> Bool
alphaEq e1 e2 = iguales (alphaEqAux e1 e2 (freeVars e1) (freeVars e2)) e1

alphaEqAux :: EAB -> EAB -> [Id] -> [Id] -> EAB
alphaEqAux (V v1) (V v2) l1 l2 = if not(v1 `pertenece` l1) && not(v2 `pertenece` l2) then V v1 else V v2
alphaEqAux (N n1) (N n2) l1 l2 = N n2
alphaEqAux (B b1) (B b2) l1 l2 = B b2
alphaEqAux (Succ e1) (Succ e2) l1 l2 = Succ e2
alphaEqAux (Pred e1) (Pred e2) l1 l2 = Pred e2
alphaEqAux (Add e1 e2) (Add e3 e4) l1 l2 = Add e3 e4
alphaEqAux (Mul e1 e2) (Mul e3 e4) l1 l2 = Mul e3 e4
alphaEqAux (Not e1) (Not e2) l1 l2 = Not e2
alphaEqAux (And e1 e2) (And e3 e4) l1 l2 = And e3 e4
alphaEqAux (Or e1 e2) (Or e3 e4) l1 l2 = Or e3 e4
alphaEqAux (Lt e1 e2) (Lt e3 e4) l1 l2 = Lt e3 e4
alphaEqAux (Gt e1 e2) (Gt e3 e4) l1 l2 = Gt e3 e4
alphaEqAux (Eq e1 e2) (Eq e3 e4) l1 l2 = Eq e3 e4
alphaEqAux (If e1 e2 e3) (If e4 e5 e6) l1 l2 = If e4 e5 e6
alphaEqAux (Let id1 e1 e2) (Let id2 f1 f2) l1 l2 = (Let id1 (alphaEqAux e1 f1 l1 l2) (alphaEqAux e2 f2 l1 l2))

-- Optimizaciones:

-- 4. Función aFolding que reciba una expresión del lenguaje EAB y regrese la expresión resultante de aplicarle plegado constante a la expresión.
aFolding :: EAB -> EAB
aFolding (V x) = (V x)
aFolding (N n) = (N n)
aFolding (B n) = (B n)
aFolding (Succ e) = case aFolding e of N n -> N (n+1)
                                       e1 -> Succ e1
aFolding (Pred e) = case aFolding e of N n -> N (n-1)
                                       e1 -> Pred e1
aFolding (Add e1 e2) = case (aFolding e1, aFolding e2) of (N n, N m) -> N (n+m)
                                                          (e11, e22) -> Add e11 e22
aFolding (Mul e1 e2) = case (aFolding e1, aFolding e2) of (N n, N m) -> N (n*m)
                                                          (e11, e22) -> Mul e11 e22
aFolding (Not e1) = Not (aFolding e1)
aFolding (And e1 e2) = And (aFolding e1) (aFolding e2)
aFolding (Or e1 e2) = Or (aFolding e1) (aFolding e2)
aFolding (Lt e1 e2) = Lt (aFolding e1) (aFolding e2)
aFolding (Gt e1 e2) = Gt (aFolding e1) (aFolding e2)
aFolding (Eq e1 e2) = Eq (aFolding e1) (aFolding e2)
aFolding (If e1 e2 e3) = If (aFolding e1) (aFolding e2) (aFolding e3)
aFolding (Let id e1 e2) = Let id (aFolding e1) (aFolding e2)

-- 5. función bFolding que reciba una expresión del lenguage EAB y le aplique el plegado constante además de eliminar ramas "muertas" de las expresiones if, 
-- es decir, si tenemos if True a else b esta expresión puede reducirse a simplemente a.
bFolding :: EAB -> EAB
bFolding (V x) = (V x)
bFolding (N n) = (N n)
bFolding (B n) = (B n)
bFolding (Succ e) = case bFolding e of N n -> N (n+1)
                                       e1 -> Succ e1
bFolding (Pred e) = case bFolding e of N n -> N (n-1)
                                       e1 -> Pred e1
bFolding (Add e1 e2) = case (bFolding e1, bFolding e2) of (N n, N m) -> N (n+m)
                                                          (e11, e22) -> Add e11 e22
bFolding (Mul e1 e2) = case (bFolding e1, bFolding e2) of (N n, N m) -> N (n*m)
                                                          (e11, e22) -> Mul e11 e22
bFolding (Not e) = case bFolding e of B b -> B (not(b))
                                      e1 -> Not e1
bFolding (And e1 e2) = case (bFolding e1, bFolding e2) of (B n, B m) -> if not n || not m then B False else B True -- Si ninguno es Falso entonces son Verdaderos y, True ^ True = True.
                                                          (B n, e22) -> if not n then B False else e22 -- n es True, y True ^ x = x
                                                          (e11, B m) -> if not m then B False else e11 -- m es True, y True ^ x = x
                                                          (e11, e22) -> And e11 e22
bFolding (Or e1 e2) = case (bFolding e1, bFolding e2) of (B n, B m) -> if n || m then B True else B False -- Si ninguno es Verdadero entonces son Falso y, Falso v Falso = Falso.
                                                         (B n, e22) -> if n then B True else e22 -- n es False, y False v x = x
                                                         (e11, B m) -> if m then B True else e11 -- m es False, y False v x = x
                                                         (e11, e22) -> Or e11 e22
bFolding (Lt e1 e2) = case (bFolding e1, bFolding e2) of (N n, N m) -> B (n<m)
                                                         (e11, e22) -> Lt e11 e22
bFolding (Gt e1 e2) = case (bFolding e1, bFolding e2) of (N n, N m) -> B (n>m)
                                                         (e11, e22) -> Gt e11 e22
bFolding (Eq e1 e2) = case (bFolding e1, bFolding e2) of (N n, N m) -> B (n==m)
                                                         (e11, e22) -> Eq e11 e22
bFolding (If e1 e2 e3) = case (bFolding e1, bFolding e2, bFolding e3) of (B b, e22, e33) -> if b then e22 else e33
                                                                         (e11, e22, e33) -> If e11 e22 e33
bFolding (Let id e1 e2) = Let id (bFolding e1) (bFolding e2)

-- Funciones auxiliares:

-- Función que elimina los elementos repetidos de una lista.
toSet :: (Eq a) => [a] -> [a]
toSet [] = []
toSet (x:xs) 
  | pertenece x xs = toSet xs
  | otherwise = x:(toSet xs)

-- Función que ve si un elemento pertenece a una lista.
pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) 
  | e == x = True
  | otherwise = pertenece e xs

-- Función que elimina un elemento de la lista.
elimina :: (Eq a) => a -> [a] -> [a]
elimina _ [] = []
elimina x (y:ys) 
  | x == y = elimina x ys
  | otherwise = y:elimina x ys

-- Función que busca si un id está en un id de la EAB Let.
buscaVarLet :: EAB -> String -> Bool
buscaVarLet (V id) s = False
buscaVarLet (N _) s = False
buscaVarLet (B _) s = False
buscaVarLet (Succ e) s = buscaVarLet e s
buscaVarLet (Pred e) s = buscaVarLet e s
buscaVarLet (Add e1 e2) s = buscaVarLet e1 s || buscaVarLet e2 s
buscaVarLet (Mul e1 e2) s = buscaVarLet e1 s || buscaVarLet e2 s
buscaVarLet (Not e) s = buscaVarLet e s
buscaVarLet (And e1 e2) s = buscaVarLet e1 s || buscaVarLet e2 s
buscaVarLet (Or e1 e2) s = buscaVarLet e1 s || buscaVarLet e2 s
buscaVarLet (Lt e1 e2) s = buscaVarLet e1 s || buscaVarLet e2 s
buscaVarLet (Gt e1 e2) s = buscaVarLet e1 s || buscaVarLet e2 s
buscaVarLet (Eq e1 e2) s = buscaVarLet e1 s || buscaVarLet e2 s
buscaVarLet (If e1 e2 e3) s = buscaVarLet e1 s || buscaVarLet e2 s || buscaVarLet e3 s
buscaVarLet (Let id e2 e3) s = s == id || buscaVarLet e2 s || buscaVarLet e3 s

-- Función que determina si dos EAB son iguales.
iguales :: EAB -> EAB -> Bool
iguales (V v1) (V v2) = v1 == v2
iguales (N v1) (N v2) = v1 == v2
iguales (B v1) (B v2) = v1 && v2
iguales (Succ e1) (Succ e2) = iguales e1 e2
iguales (Pred e1) (Pred e2) = iguales e1 e2
iguales (Add e1 e2) (Add e3 e4) = (iguales e1 e3) && (iguales e2 e4)
iguales (Mul e1 e2) (Mul e3 e4) = (iguales e1 e3) && (iguales e2 e4)
iguales (Not e1) (Not e2) = iguales e1 e2
iguales (And e1 e2) (And e3 e4) = (iguales e1 e3) && (iguales e2 e4)
iguales (Or e1 e2) (Or e3 e4) = (iguales e1 e3) && (iguales e2 e4)
iguales (Lt e1 e2) (Lt e3 e4) = (iguales e1 e3) && (iguales e2 e4)
iguales (Gt e1 e2) (Gt e3 e4) = (iguales e1 e3) && (iguales e2 e4)
iguales (Eq e1 e2) (Eq e3 e4) = (iguales e1 e3) && (iguales e2 e4)
iguales (If e1 e2 e3) (If e4 e5 e6) = (iguales e1 e4) && (iguales e2 e5) && (iguales e3 e6)
iguales (Let id1 e2 e3) (Let id2 e4 e5) = id1 == id2 && (iguales e2 e4) && (iguales e3 e5)