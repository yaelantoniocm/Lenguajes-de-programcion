{-| 
            Práctica 01

    Ulrich Villavicencio Cárdenas
    Yael Antonio Calzada Martín
-}

-- 1. Definición de los naturales:
data Nat = Cero | Suc Nat deriving Show

-- 2. Función suma que recibe dos números naturales n y m y regresa un número Natural k que representa la suma de n y m:
suma :: Nat -> Nat -> Nat
suma n m = case m of Suc x -> suma (Suc n) x
                     _ -> n
                     
-- 3. Función sub que recibe dos números naturales n y m y regresa cero si sucede que n ≤ m o un número Natural k que representa la resta de n y m.
sub :: Nat -> Nat -> Nat
sub Cero n = Cero
sub n Cero = n
sub (Suc n) (Suc m) = sub n m

-- 4. Función mul que recibe dos números naturales n y m y regresa un número Natural k que representa la multiplicación de n y m.
mul :: Nat -> Nat -> Nat
mul Cero m = Cero
mul n Cero = Cero
mul n m = mulAux n m n

mulAux :: Nat -> Nat -> Nat -> Nat
mulAux n m a = case m of Suc Cero -> a
                         Suc x -> mulAux n x (suma a n)
                         
-- 5. Función menorQue que recibe dos números naturales n y m y regresa un booleano que indica si el primer argumento es menor al segundo.
menorQue :: Nat -> Nat -> Bool
menorQue n Cero = False
menorQue Cero n = True
menorQue (Suc n)(Suc m) = menorQue n m 

-- 6. Función eq que recibe dos números naturales n y m y regresa un booleano que indica si n representa el mismo número que m.
eq :: Nat -> Nat -> Bool
eq n m = case n of Suc x -> case m of Suc y -> eq x y
                                      Cero -> False
                   Cero -> case m of Cero -> True
                                     _ -> False

-- 7. Función par que recibe un número Natural n y regresa un booleano que indica si n es par.
par :: Nat -> Bool
par Cero = True
par (Suc Cero) = False
par (Suc (Suc n)) = par n

-- 8. Función impar que recibe un número Natural n y regresa un booleano que indica si n es impar
impar :: Nat -> Bool
impar n
    | (par n == True) = False
    | otherwise = True
    
--9. Función toInt que transforma un número Natural a un número entero de Haskell.
toInt :: Nat -> Int
toInt Cero = 0
toInt (Suc n) = 1 + toInt n

-- 10. Una función parcial es aquella que no esta definida sobre todo su dominio. La función que transforme un número
--     entero a un número Natural es una función parcial pues no hay manera de transformar un número negativo a un
--     número Natural.

-- 10.1. Función toNat que transforma un número entero de Haskell a un número Natural.
toNat :: Int -> Nat
toNat x
    | x < 0 = error "Por favor ingrese un número Natural."
    | otherwise = toNatAux x Cero

toNatAux :: Int -> Nat -> Nat
toNatAux x n
    | (x == 0) = n
    | otherwise = toNatAux (x-1) (Suc n) 

--10.2. El problema de usar la función error es que provoca que la evaluación se detenga. Una manera de evitar
--esto es usando el tipo de dato Maybe. Modifica la función Natural usando el tipo de dato Maybe para manejar el
--caso de los números negativos.
toNatMaybe :: Int -> Maybe Nat
toNatMaybe n
    | n < 0     = Nothing
    | otherwise = Just (toNat n)

-- Conjuntos

--Tipo de dato Set definido como un sinónimo del tipo de dato Lista.
type Set a = [a]

-- 1. Función pertenece que recibe un elemento y regresa un booleano indicando si el elemento existe en el
-- conjunto.
pertenece :: ( Eq a ) => a -> Set a -> Bool
pertenece e [] = False
pertenece e (x:xs) 
    | e == x = True
    | otherwise = pertenece e xs

-- 2. Función esConjunto que determina si un elemento del tipo Set es un conjunto según la definición dada.
esConjunto :: (Eq a) => Set a -> Bool
esConjunto [] = True
esConjunto (x:xs) 
    | (x `pertenece` xs) = False
    | otherwise = esConjunto xs 

--3. Función toSet que reciba una lista y devuelve un conjunto.
toSet :: (Eq a ) => [ a ] -> Set a 
toSet [] = []
toSet (x:xs) 
    | pertenece x xs = toSet xs
    | otherwise = x:(toSet xs)
  
-- 4. Función eq que recibe dos conjuntos y determine si son iguales
eqSet :: (Eq a) => Set a -> Set a -> Bool
eqSet xs ys 
    | ((xs == []) && (ys /= [])) || (xs /= []) && (ys == []) = False 
    | otherwise = (eqIzq xs ys) && (eqIzq ys xs)

eqIzq :: (Eq a) => Set a -> Set a -> Bool
eqIzq [] ys = True
eqIzq (x:xs) ys
    | (x `pertenece` ys) = eqIzq xs ys
    | otherwise = False

--5. En Haskell las funciones son de primer orden, es decir, podemos recibir funciones como parámetros y regresar funciones.
-- Función todos que recibe una función f y un conjunto A y determina si todos los elementos de A cumplen f.
todos :: (Eq a ) => ( a -> Bool ) -> Set a -> Bool
todos f [] =  False
todos f (x:xs) = todosAux f (x:xs)

todosAux :: (Eq a ) => ( a -> Bool ) -> Set a -> Bool
todosAux f [] =  True
todosAux f (x:xs) 
    | f x = todosAux f xs 
    | otherwise = False
   
-- 6. Función alguno que recibe un conjunto A, una función f y determina existe un elemento de A que cumple f.
alguno :: (Eq a) => Set a -> (a -> Bool) -> Bool
alguno [] f = False
alguno (x:xs) f
    | f x = True
    | otherwise = alguno xs f

--7. Función agrega que agrega un elemento a un conjunto. Si el elemento ya existe entonces no es necesario hacer nada.
agrega :: (Eq a ) => a -> Set a -> Set a
agrega n [] = [n]
agrega n xs 
    | pertenece n xs = xs
    | otherwise = n:xs

-- 8. Función union que recibe dos conjuntos y regresa la unión de ellos.
union :: (Eq a) => Set a -> Set a -> Set a
union [] ys = ys
union xs [] = xs
union (x:xs) ys = union xs (x `agrega` ys)

-- 9. Función interseccion que recibe dos conjuntos y regresa la intersección de ellos.
interseccion :: (Eq a ) => Set a -> Set a -> Set a
interseccion [] cs = []
interseccion (x:xs) cs
    | x `pertenece` cs = x:(interseccion xs cs)
    | otherwise = interseccion xs cs
    
-- 10. Función diferencia que recibe un conjunto A, un conjunto B y regresa un conjunto C que contiene los elementos de A que no están en B.
diferencia :: (Eq a) => Set a -> Set a -> Set a
diferencia [] ys = []
diferencia xs [] = xs
diferencia xs ys = [z | z <- xs, not (z `pertenece` ys)]

-- 11. Función subconjunto que recibe un conjunto S y un conjunto C y verifica si S es subconjunto de C.
esSubconjunto :: (Eq a ) => Set a -> Set a -> Bool
esSubconjunto [] n = True
esSubconjunto (x:xs) cs = x `pertenece` cs && esSubconjunto xs cs
