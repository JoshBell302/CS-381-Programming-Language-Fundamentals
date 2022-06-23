-- Homework 3



-- Exercise 1. A Stack Language

type Prog = [Cmd]

data Cmd = LD Int
    | ADD
    | MULT
    | DUP

type Stack = [Int]

----------------------------------------------
-- Tests :	map sem
-- sem [LD 3,DUP,ADD,DUP,MULT] []
-- sem [LD 3,ADD] []
-- sem [] []
----------------------------------------------

type D = Stack -> Maybe Stack

sem :: Prog -> D
sem c [-1] = Nothing
sem [] [] = Nothing
sem [] s = Just s
sem (x:xs) s = case sem xs (stackCon(semCmd x s)) of
    Just [x] -> Just [x]
    Nothing -> Nothing

semCmd :: Cmd -> D
semCmd (LD i) s = Just (s ++ [i])
semCmd (ADD) s = case reverse s of
    [] -> Nothing
    [s] -> Nothing
    (s:ss) -> Just (tail(tail(s:ss)) ++ [s + (head ss)])
semCmd (MULT) s = case reverse s of
    [] -> Nothing
    [s] -> Nothing
    (s:ss) -> Just (tail(tail(s:ss)) ++ [s * (head ss)])
semCmd (DUP) s = case reverse s of
    [] -> Nothing
    [s] -> Just ([s,s])
    (s:ss) -> Just ((s:ss) ++ [s])

stackCon :: Maybe Stack -> Stack
stackCon (Just m) = m
stackCon (Nothing) = [-1]




-- Exercise 2. Mini Logo

data Cmd' = Pen Mode
    | MoveTo Int Int
    | Seq Cmd' Cmd'

data Mode = Up | Down

type State = (Mode, Int, Int)

type Line  = (Int, Int, Int, Int)

type Lines = [Line]

----------------------------------------

sem' :: Cmd' -> Lines
sem' c = snd semS c (Up,0,0)

semS :: Cmd' -> State -> (State,Lines)
semS (Pen m) (_ x y) = ((m x y), [])
semS (MoveTo nx ny) (m x y)
    | m == Down = ((m nx, ny),[(x, y, nx, ny)])
    | Otherwise = ((m nx, ny), [])
semS (Seq c1 c2) state@(m x y) = (c, b ++ d)
    where (a, b) = semS c1 state
    (c, d) = semS c2 b


















