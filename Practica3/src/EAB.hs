{- | 
      Práctica 03
  Yael Antonio Calzada Martin
-}

module EAB where 

type Id = String 
data EAB = V Id | N Int | B Bool
        | Succ EAB | Pred EAB
        | Plus EAB EAB | Mult EAB EAB | IsZero EAB
        | If EAB EAB EAB | Let Id EAB EAB





