{-# LANGUAGE ExistentialQuantification #-}

import Data.Array

data Var = Var String
type Aval = Int
-- data Aval = forall a. (Num a, Ord a) => Aval a -- TODO: Nush daca forall e satisfacator. Multa info pe partea asta
data Bval = True | False
data AExpr = AEXvar Var | AEXaval Aval | Adun AExpr AExpr | ParanA AExpr
data BExpr = BEX Bval | AND BExpr BExpr | Greater AExpr AExpr | ParanB BExpr
data Stmt = Var := AExpr | Acol Stmt | If BExpr Stmt Stmt | While BExpr Stmt | Legatura Stmt Stmt
data Varlist = Var1 Var | Varmany Var Varlist
data Prog = Initial Varlist Stmt 

-- TODO: modifica
data ADT = NIL | Node ADT Stmt ADT

instance Eq Bval where

    Main.True == Main.True = Prelude.True
    Main.False == Main.False = Prelude.False

--instance Num Aval where

    -- nu stie daca tipul lui x este tipul lui y
    -- TODO : rezolva cumva
    --(+) :: (Aval a) => a -> a -> a
--    (Aval (Num a => x)) + (Aval y) | (x => (Num a) && y => (Num a)) = Aval (x + y)
    -- ?


--instance Ord Aval where

--    (Aval x) > (Aval y) | x > y = Prelude.True
--                        | otherwise = Prelude.False

program :: String

-- exemplu program
program = "Int a, b; a = 100; b = 50;" 


evalAExpr :: AExpr -> Aval

--Todo:: add pt variabile
--evalAExpr

evalAExpr (AEXaval aval) = aval

evalAExpr (Adun ax1 ax2) = ((evalAExpr ax1) + (evalAExpr ax2)) 

evalAExpr (ParanA ax1) = evalAExpr ax1 

evalBExpr :: BExpr -> Bval

evalBExpr (BEX val) = val

--short circuit
evalBExpr (AND ex1 ex2) = if (evalBExpr ex1) == Main.False then
                         Main.False else (if (evalBExpr ex2 == Main.False) then
                                             Main.False else
                                             Main.True)
--TODO: AExpr interface pt ord
evalBExpr (Greater ex1 ex2) = if (evalAExpr ex1) > (evalAExpr ex2) then
                                Main.True else
                                Main.False

evalBExpr (ParanB ex1) = evalBExpr ex1


