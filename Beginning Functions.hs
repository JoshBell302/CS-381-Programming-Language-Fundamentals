import Prelude hiding (sum)
import Prelude hiding (length)
import Prelude hiding (even)

-- Retruns the first element in a list === head [1,2,3] => 1
head :: [a] -> a
head (x:_) = x
						
-- Returns a list without the first element === tail [1,2,3] => [2,3]
tail :: [a] -> [a]
tail (_:xs) = xs
						
-- Adds all Ints in the list together === sum [1,2,3] => 6
--sum :: [Int] -> Int
--sum [] = 0
--sum (x:xs) = x + sum xs

-- Adds the number of Ints in the list === length [1,2,3] => 3
--length :: [a] -> Int
--length [] = 0
--length (_:xs) = 1 + length xs

-- Doubles the value of the given Int === dbl 4 => 8
dbl :: Int -> Int
dbl x = 2 * x

-- Adds two Ints together === plus 3 4 => 7
-- map (plus 3) [1,2,3] => [4,5,6]
plus :: Int -> Int -> Int
plus x y = x+y

-- Adds two Ints together === plus' (3,4) => 7
plus' :: (Int,Int) -> Int
plus' (x,y) = x+y

-- Checks to see which Int is bigger === larger 5 6 => 6
-- larger 6 5 => 6
larger :: Int -> Int -> Int
larger x y = if x >= y then x else y

-- Flips the boolean value === not True => False
not :: Bool -> Bool
-- not x = if x == True if True then False else True
-- not x = if x then False else True
not True = False
not False = True

-- Checks the sign if the given Int and returns a 1 if positive, -1 if negative, and 0 if zero
-- sign 3 => 1
-- sign (-5) => -1
-- sign 0 => 0
sign :: Int -> Int
sign x	| x>0 = 1
	| x<0 = -1
	| otherwise = 0

-- Checks to see if value is even === even 2 => True
-- even 3 => False
--even :: Int -> Bool
--even 0 = True
--even 1 = False
--even x = even (x-2)

-- Gives the factorial value of the given Int === fac 4 => 24
fac :: Int -> Int
fac 1 = 1
fac x = x * fac (x-1)

addMe :: Int -> Int -> Int
addMe x y = x + y