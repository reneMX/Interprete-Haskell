module InterpF where

import Grammars3

interp :: ASA -> ASA
interp (Num n) = (Num n)
interp (Id i) = error "Variable no definida"
interp (Suma i d) = Num (numN (interp i) + numN(interp d))
interp (Resta i d) = Num (numN (interp i) - numN (interp d))
interp (Mult i d) = Num (numN (interp i) * numN (interp d))
interp (Div i d) = let iv = (interp i)
                       dv = (interp d) in
    if numN(dv) /= 0 then Num(div (numN iv) (numN dv))
               else error "División por cero"
interp (Let i val cuerpo) = interp (sust cuerpo i (interp val))
interp (Lambda param cuerpo) = (Lambda param cuerpo)
interp (App(Lambda p c) a) = interp(sust c p (interp a))

numN :: ASA -> Int
numN (Num n) = n

sust :: ASA -> String -> ASA -> ASA
sust (Num n) id val = (Num n)
sust (Id i) id val = if (id == i) then val else (Id i)
sust (Suma i d) id val = Suma (sust i id val) (sust d id val)
sust (Resta i d) id val = Resta (sust i id val) (sust d id val)
sust (Mult i d) id val = Mult (sust i id val) (sust d id val)
sust (Div i d) id val = Div (sust i id val) (sust d id val)
sust (Let i v c) id val 
  | id == i = Let i (sust v id val) c
  | id /= i = Let i (sust v id val) (sust c id val)
sust (Lambda param cuerpo) id val
  | param == id = (Lambda param cuerpo)
  | otherwise = (Lambda param (sust cuerpo id val))
sust (App f a) id val = (App (sust f id val) (sust a id val)) 

