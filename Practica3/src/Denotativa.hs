{- | 
      Práctica 03
  Yael Antonio Calzada Martin
-}
module Denotativa where
import EAB 
data Value = Nat Int | Boolean Bool

--1.Define la función dEval que recibe una expresión del lenguaje EAB y devuelve el valor asociado a ésta.
--  La manera en como se debe hacer el paso de parámetros debe ser mediante sustitución.
--  En nuestro lenguaje los posibles valores son los números naturales y los booleanos, esto lo podemos modelar 
--  en Haskell mediante el tipo de dato Value definido de la siguiente manera.

dEval :: EAB -> Value  
dEval (V v) = error "in this expression there are variables"
dEval (N n) = Nat n 
dEval (B b) = Boolean b
dEval (Succ e) = (Succ (dEval e))
dEval (Pred e) = (Pred (dEval e))
dEval (Mult e1 e2) = dEval e1 + dEval e2
dEval (IsZero e) = case dEval e of Nat 0 -> Boolean True
                                   _ -> False
dEval 
    



--2. Define la función dEval2 que recibe una expresión del lenguaje EAB y devuelve el valor asociado a ésta.
--   La manera es como se debe hacer el paso de parámetros debe ser haciendo el uso de un estado.
type State = Id -> Value 

dEva2 :: State -> EAB -> Value

 



























