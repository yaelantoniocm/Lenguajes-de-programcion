{- | 
      Práctica 03
  Yael Antonio Calzada Martin
-}
module Dinamica where
import EAB
import Practica2
--11. Define la función eval1 de manera que eval1 e = e’ syss e -> e’
eval1 :: EAB -> EAB
eval1 (V v) = (V v)
eval1 (N n) = (I n)
eval1 (B True) = (B True)
eval1 (B False) = (B False)
eval1 (Succ (N e)) = (N (e + 1))
eval1 (Succ x) = (Succ (eval1 x))
eval1 (Pred (N e)) = (N (e - 1))
eval1 (Pred x) = (Pred (eval1 x))
eval1 (Plus (N e1)(N e2)) = (N (e1 + e2))
eval1 (Plus (N e1) e2) = (Plus (N e1) (eval1 e2))
eval1 (Plus e1 e2) = (Plus (eval1 e1) e2)
eval1 (Mult (N e1)(N e2)) = (N (e1 * e2))
eval1 (Mult (N e1) e2) = (Mult (N e1) (eval1 e2))
eval1 (Mult e1 e2) = (Mult (eval1 e1) e2)
eval1 (IsZero e) = (IsZero e)
--eval1 (If e1 e2 e3) = 






--12. Define la función evals de manera que evals e = e’ syss e -> ∗ e’ y e’ es un estado bloqueado
evals :: EAB -> EAB
evals (V v) = (V v)
evals (N n) = (I n)
evals (B True) = (B True)
evals (B False) = (B False)
evals (Succ (N e)) = (N (n + 1))
evals (Succ x) = evals (Succ (evals x))
evals (Pred (N e)) = (N( e - 1))
evals (Pred x) = evals (Pred (evals x))
evals (Plus (N e1) (N e2)) = (N (e1 + e2))
evals (Plus (N e1) x) = evals (Plus (N e1) (evals m))
evals (Plus x y) = evals (Plus (evals x)(evals y))
evals (Mult (N e1)(N e2)) = (N(e1 * e2))
evals (Mult (N e1) x) = evals (Mult (N e1) evals(x))
evals (Mult x y) = evals (Mult (evals x) (evals y))
evals (IsZero e) = (IsZero e) 
--evals (If e1 e2 e30) = 
evals (Let x e1 e2) = (substitution (e2) (x, (evals e1)))




--13. Define la función isValid de manera que evals e = e’ syss e -> ∗ e’ y e’ es un valor
--isValid :: EAB -> Bool

