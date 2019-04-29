-- =============================================================================== --
{- |
  Welcome to your fourth Haskell training. Get ready to rumble.
  Where will you let recursion lead you?

  You should know the drill by now - solve the exercises in `TrainingExercises.hs`,
  run the tests with Cabal, push to `training-04`, create a Merge Request,
  and assign it to your TA.

  Keep in mind that **all exposed functions must have type signatures** (helper functions that are defined in local definitions don't)!

  As always, ask your TA if you need any help.
-}
-- =============================================================================== --
--
module TrainingExercises where
--
import Data.List
import Data.Char
--

{- * 4.1 Recursive functions -}

-- ** TE 4.1.1
--
-- | Define a recursive function that calculates the minimal number of moves needed to complete a game
-- | of Towers of Hanoi with n disks. 
-- | 
-- | In case you don't know about that game, take a look here: 
-- | https://en.wikipedia.org/wiki/Tower_of_Hanoi#Recursive_solution
--

te411 :: Integer -> Integer
te411 n = hanoi n "a" "b" "c"
  where hanoi 1 _ _ _ = 1
        hanoi n a b c = hanoi (n-1) a c b + 1 + hanoi (n - 1) c b a


-- ** TE 4.1.2
--
-- | Define a recursive function that calculates the greatest common divisor of two given numbers.
--

te412 :: Integer -> Integer -> Integer
te412 a 0 = a
te412 a b = te412 b (a `mod` b) 


-- ** TE 4.1.3
--
-- | Define a recursive function that returns the last element of a list.
-- | What do you think should happen with an empty list?
--

te413 :: [a] -> a
te413 []     = error "No last element on empty list"
te413 [a]    = a
te413 (x:xs) = te413 xs


-- ** TE 4.1.4
--
-- | You have seen a Quick Sort implementation on the lecture. Now is the time to implement Merge Sort.
-- | You are not allowed to use list comprehension here!
--

te414 :: Ord a => [a] -> [a]
te414 []  = []
te414 [x] = [x]
te414 xs  = merge (te414 a) (te414 b)
  where l           = length xs
        h           = l `div` 2
        (a, b)      = splitAt h xs
        merge xs [] = xs
        merge [] xs = xs 
        merge (x:xs) (y:ys) 
          | (y > x)   = x:(merge xs (y:ys))
          | otherwise = y:(merge ys (x:xs))

-- ** TE 4.1.5 - EXTRA
--
-- | Now you have written 2 different efficient sorting algorithms. Let's write something worse!
-- | Write an Insertion sort function.
-- | List comprehensions are not alowed once again! 
--

te415 :: Ord a => [a] -> [a]
te415 []     = []
te415 [x]    = [x]
te415 (x:xs) = merge x (te415 xs)
  where merge x []     = [x]
        merge x (y:ys)
          | y > x     = x:y:ys
          | otherwise = y:(merge x ys)

  {- * 4.2 Corecursion -}

-- ** TE 4.2.1
--
-- | Write your own definition of the cycle function.
--

te421 :: [a] -> [a]
te421 xs = xs' 
  where xs' = xs ++ xs'
