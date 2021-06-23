{- | 
      Práctica 03
  Yael Antonio Calzada Martin
-}
module Traspiladores where
import EAB
type Id = String
data EB1 = Tt1 | Ff1
			| VarB1 Id	
			| Not EB1 | And EB1 EB1
			| Let Id EB1 EB1
			deriving ( Show , Eq )

data EB2 = Tt2 | Ff2
			| VarB2 Id
			| If EB2 EB2 EB2
			| Where EB2 Id EB2
			deriving ( Show , Eq )

--3. Defina la función toEB2 que recibe una expresión del lenguaje EB1 y regresa una expresión equivalente del
--   lenguaje EB2. En los comentarios de la función explique la equivalencia de las expresiones.

toEB2 :: EB1 -> EAB2


--4. Defina la función toEB1 que recibe una expresión del lenguaje EB2 y regresa una expresión equivalente del
--	lenguaje EB1. En los comentarios de la función explique la equivalencia de las expresiones.

toEB1 :: EB2 -> EB1 


--5. Defina un interprete para el lenguaje EB1 dado por la función eval1. El paso de parámetros deberá ser
--   mediante sustitución 
eval1 :: EB1 -> EB1

--6. Defina un interprete para el lenguaje EB1 dado por la función eval2. El paso de parámetros deberá ser
--  mediante el uso de un estado.
eval2 :: State -> EB1 -> EB1 


--7. Defina un interprete para el lenguaje EB2 dado por la función eval3. El paso de parámetros deberá ser
--   mediante sustitución 
eval3 :: EB2 -> EB2



--8. Defina un interprete para el lenguaje EB2 dado por la función eval4. El paso de parámetros deberá ser
--   mediante el uso de un estado.
eval4 :: State -> EB2 -> EB2



--9. Defina la función evalEB1 que evalue una expresión del lenguaje EB1 haciendo una transpilación a EB2
--   y luego evaluando la expresión resultante.
evalEB1 :: EB1 -> Bool


--10. Defina la función evalEB2 que evalue una expresión del lenguaje EB1 haciendo una transpilación a EB2
--    y luego evaluando la expresión resultante.
evalEB2 :: EB2 -> Bool



