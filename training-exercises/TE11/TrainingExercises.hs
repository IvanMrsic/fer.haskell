-- =============================================================================== --
{- |
  Welcome to your eleventh Haskell training.

  You should know the drill by now - solve the exercises in `TrainingExercises.hs`,
  run the tests with Cabal, push to `training-11`, create a Merge Request,
  and assign it to your TA.

  Keep in mind that **all exposed functions and instance definitions must have
  type signatures**.
  (helper functions that are defined in local definitions don't)!

  Do not change the function names or type signatures.

  There aren't any extra tasks - all tasks are mandatory.

  As always, ask your TA if you need any help.
-}
-- =============================================================================== --
--
module TrainingExercises where
--
import Data.Char
import Data.List
import Data.Ord
import qualified Data.Set as S
import qualified Data.Map as M
import System.Random
--

{- * 11.1 Recursive data types and type class instances  -}

-- ** TE 11.1.1

-- Using 'foldl' from Foldable define a function that
-- returns the product of all elements greater than zero.
-- Example: product' [4, 0, 3, -9, 1] => 12

product' :: (Foldable t, Num a, Ord a) => t a -> a
product' = foldl (\x s -> if s > 0 then x * s else x) 1


{- 11.2 Standard data types -}

-- ** TE 11.2.1

-- Find the index of the first duplicate element in a list.
-- If there is no duplicate element, return -1.
--
-- This time, take into consideration time complexity and utilise the
-- built-in set data structure.
--
-- Example: firstDup [5, 2, 3, 5] = 3

firstDup :: Ord a => [a] -> Int
firstDup xs = findDupIndex xs S.empty 0 
  where findDupIndex []     _ _ = -1
        findDupIndex (x:xs) s i =  if x `S.member` s then i else findDupIndex xs (S.insert x s) (i + 1)


-- ** TE 11.2.2

-- Check if the two lists are permutations of each other.
-- You must solve this by using the dictionary data structure.
-- Hint: You should definitely write a helper function here.
-- Example: isPermutation [3, 2, 1] [2, 1, 3] = True
--          isPermutation [5, 2, 3] [5, 3, 3] = False

count ::  Ord a => [a] -> M.Map a Int
count = M.fromList . map (\x -> (head x, length x)) . group . sort 

isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation xs ys = (count xs) == (count ys)

{- 11.3 IO and random -}

-- ** TE 11.3.1

-- Write a function that will read integers from standard input until
-- a duplicate number is read. After that, write "goodbye" to stdout.
-- Example (stdin):
--   3
--   7
--   -1
--   20
--   5
--   3
-- Stdout: goodbye

askNumber :: [Int] -> IO ()
askNumber xs = do
  input <- getLine
  let x = (read input :: Int)
  if x `elem` xs then putStrLn "goodbye"
   else askNumber (x:xs)   

firstDupIO :: IO ()
firstDupIO = askNumber []


-- ** TE 11.3.2

-- Since we didn't cover RNGs in class, this task should give you a basic
-- sense of how RNGs in Haskell work.

-- Create a function that reads an integer N from standard input and
-- generates N random integers that are printed to standard output.

-- Here's some help: inside an IO, you can get a random number generator with
-- `getStdGen`. To generate random values, use `randoms` function. It accepts a
-- single parameter - a random number generator you got from `getStdGen`.
-- You ought to enforce it to return Ints. How?

-- Example (stdin):
--   4
-- Stdout:
--   -1460913578
--   1231
--   420
--   1337
-- (all of these are completely random!)

userRandomIO :: IO ()
userRandomIO = do
  input <- getLine
  let x = (read input :: Int)
  g <- getStdGen 
  mapM_ print (take x (randoms g :: [Int]))