--Homework 2



-- Exercise 1. Mini Logo
-- (a) Define the abstract syntax for Mini Logo as a Haskell data type.
    

-- (b) Write a Mini Logo macro vector that draws a line from a given position (x1,y1) to a given position (x2,y2) and represent the macro in abstract syntax, that is, as a Haskell data type value.


-- (c) Define a Haskell function steps :: Int -> Cmd that constructs a Mini Logo program which draws a stair of n steps. Your solution should not use the macro vector.



-- Exercise 2. Digital Circuit Design Language
-- (a) Define the abstract syntax for the above language as a Haskell data type.

import Prelude hiding (Num)
type Num = Int

data Circuit = C Gates Links
data Gates = G Num GateFn Gates | Empty
data GateFn = And | Or | Xor | Not
data Links = From (Num,Num) (Num,Num) Links | None


-- (b) Represent the half adder circuit in abstract syntax, that is, as a Haskell data type value.

halfAdder =  C (G 1 Xor (G 2 And (Empty))) (From (1,1) (2,1) ( From (1,2) (2,2) (None)))


-- (c) Define a Haskell function that implements a pretty printer for the abstract syntax.

ppCircuit :: Circuit -> String
ppCircuit (C g l) = ppGates g ++ ppLinks l

ppGates :: Gates -> String
ppGates (G n f Empty) = "{"++ show n ++ ":" ++ ppGateFn f ++ "}  "
ppGates (G n f g) = "{"++ show n ++ ":" ++ ppGateFn f ++ "} " ++ ppGates g

ppGateFn :: GateFn -> String
ppGateFn (And) = "And"
ppGateFn (Or) = "Or "
ppGateFn (Xor) = "Xor"
ppGateFn (Not) = "Not"

ppLinks :: Links -> String
ppLinks (From (a,b) (c,d) None) = "From " ++ show a ++ "." ++ show b ++ " to " ++ show c ++ "." ++ show d
ppLinks (From (a,b) (c,d) l) = "From " ++ show a ++ "." ++ show b ++ " to " ++ show c ++ "." ++ show d ++ " " ++ ppLinks l

--ppCircuit halfAdder
