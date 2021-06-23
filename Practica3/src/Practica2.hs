-- De la practica 2 usaremos substitution
module Practica2 where
type Substitution = (Id , EAB)
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

--freeVars :: EAB -> [Id]
--freeVars e = case e of
    --(V v) -> [(V v)]
    --(N n) -> []
    --(B b) -> []
    --(Add e1 e2) ->  (freeVars e1) ++ (freeVars e2)
    --(Mul e1 e2) ->  (freeVars e1) ++ (freeVars e2)
    --(And e1 e2) ->  (freeVars e1) ++ (freeVars e2)
    --(Or e1 e2) ->  (freeVars e1) ++ (freeVars e2)
    --(Lt e1 e2) ->  (freeVars e1) ++ (freeVars e2)
    --(Gt e1 e2) ->  (freeVars e1) ++ (freeVars e2)
    --(Eq e1 e2) ->  (freeVars e1) ++ (freeVars e2)
    --(Pred n) -> (freeVars n)
    --(Succ n) -> (freeVars n)
    --(Not n) -> (freeVars n)
    --(If e1 e2 e3) ->  (freeVars e1) ++ ((freeVars e2) ++ (freeVars e3))
    --(Let x e1 e2) ->  (freeVars e1) ++ (filter (/= (V x)) (freeVars e2))



--substitution :: EAB -> Substitution -> EAB
--substitution e t@(x, y) = case e of 
	--(V v) -> if (v == x) then y else e
	--(N e1) -> e
	--(B b) -> e
	--(Succ n) -> (Succ (substitution n t))
	--(Pred n) -> (Pred (substitution n t))
	--(Add n m) -> (Add (substitution n t) (substitution m t))
	--(Mul n m) -> (Mul (substitution n t) (substitution n t))
	--(Not n) -> (Not (substitution n t)) 
	--(And n m) -> (And (substitution n t) (substitution m t))
	--(Or n m) -> (Or (substitution n t) (substitution m t))
	--(Lt n m) -> (Lt (substitution n t) (substitution m t))
	--(Gt n m) -> (Gt (substitution n t) (substitution m t))
	--(Eq n m) -> (Eq (substitution n t) (substitution m t))
	--(If e1 e2 e3) -> (If (substitution e1 t) (substitution e2 t) (substitution e3 t))
	--(Let w e1 e2) -> if (x == w) then
		                --(Let w (substitution e1 t) e2)
		            --else if ((V w `elem` (freeVars e1))) then
		            	--error "This substitution cannot be"
		            --else 
		            	--(Let x(substitution e1 t)(substitution e2 t))

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

