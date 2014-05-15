module RPN  where

import Data.List

type Stack = [Int]

type Operator = Stack -> Stack

binOp :: (Int -> Int -> Int) -> Stack -> Stack
binOp f (h : (ht :t)) = (f ht h : t)

dup (h : t) = (h : ( h :t))
swap (h : (ht : t)) = (ht : (h : t))

pick :: Stack -> Stack
pick (h : t) = (genericIndex t h : t)

parseOp :: String -> Operator
parseOp "+" = binOp (+)
parseOp "-" = binOp (-)
parseOp "*" = binOp (*)
parseOp "/" = binOp div
parseOp "dup" = dup
parseOp "swap" = swap
parseOp "drop" = tail
parseOp "depth" = (\l -> (length l : l))
parseOp "pick"  =  pick

eval :: Stack -> [Operator] -> Stack
eval s [] = s
eval s op = eval (head op s) (tail op)

--parse :: String -> [Operator]