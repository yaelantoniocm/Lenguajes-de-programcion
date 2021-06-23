{- | 
      Práctica 03
  Yael Antonio Calzada Martin
-}
module Estatica where
import EAB
type Ctx = [( Id , Type )]
--type CtxNoList =  ( Id , Type )
data Type = Nat | Boolean 

--13. Define la función vt que recibe un contexto Γ, una expresión e del lenguaje EAB y un tipo T y decide si
--    Γ ` e : T
vt :: Ctx -> EAB -> Type -> Bool
--vt ctx (V v) x= vtAux ctx (v, x)
vt _ (N n) Nat = True
vt _ (N n) Boolean = False
vt _ (B b) Nat = False
vt _ (B b) Boolean = True
vt ctx (Succ e) Nat = (vt ctx e Nat)
vt ctx (Succ e) Boolean = False
vt ctx (Pred e) Nat = (vt ctx e Nat)
vt ctx (Pred e) Boolean = False
vt ctx (Plus e1 e2) Nat = (vt ctx e1 Nat) && (vt ctx e2 Nat) 
vt ctx (Plus e1 e2) Boolean = False 
vt ctx (Mult e1 e2) Nat = (vt ctx e1 Nat) && (vt ctx e2 Nat) 
vt ctx (Mult e1 e2) Boolean = False
vt ctx (IsZero e) Nat = (vt ctx e Nat)
vt ctx (IsZero e) Boolean = False
vt ctx (If e1 e2 e3) x = (vt ctx e1 x) && (vt ctx e2 x) && (vt ctx e3 x)
vt ctx (Let y e1 e2) x 
    |(vt ctx e1 Nat) = (vt (ctx ++ [(y, Nat)]) e2 x) 
    |(vt ctx e1 Boolean) = (vt (ctx ++ [(y, Boolean)]) e2 x)


-- Funcion auxiliar que se utilizará para V en v, que nos ayudara a encontrar y decir si esta elemeto en nuestra lista Ctx en CtxNoList
--vtAux :: Ctx -> CtxNoList -> Bool
--vtAux [] _ = False
--vtAux (x:xs) n 
--      |(equalCtxNoList x n) = True
--      |otherwise = vtAux xs n

-- Funcion que compara elementos del mismo tipo y nos dice si son iguales
--equalCtxNoList :: CtxNoList -> CtxNoList -> Bool
--equalCtxNoList _ _ = False
--equalCtxNoList (n, Nat) (m, Boolean) = error "There aren't the same type"
--equalCtxNoList (n, Boolean) (m, Nat) = error "There aren't the same type"
--equalCtxNoList (n, Boolean) (m, Boolean) = (n == m)
--equalCtxNoList (n, Nat) (m, Nat) = (n == m)
