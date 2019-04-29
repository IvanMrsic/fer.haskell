-- =============================================================================== --
{- |
  Welcome to your first Haskell training. Every training exercise ( TE ) has a single
  function in the form of 'texxx', so for training exercise 1.1.1 there will be a
  function 'te111'.

  To start solving the problem simply remove the 'undefined' from the body of a
  function / definition and start writing your solution.

  Before going any further, you should try to update your package index and possibly
  your cabal (cabal is a build tool for Haskell). To update your package index run
  the following command:

  >> cabal new-update

  If that's not working, try

  >> cabal update
  >> cabal install cabal-install

  This will install the latest version of `cabal` which has `new-` commands available.

  To test your solutions you can open the repl / ghci by opening the terminal in the
  folder of your training (e.g. 'C:\Users\minime\Desktop\TE01' if you are on Windows
  or ~/Desktop/TE01 if you are on a Unix system), and typing in the following command
  (without the '>'):

  >> cabal new-repl

  This will download the necessary dependencies (if there are any) and load your
  'TrainingExercises' module into the repl where you can try to call your functions
  with different arguments.

  When you make changes to your code you can type ':r' into the repl to reload your
  code.

  For this training, we have also included the test suite so that you can quickly
  check if your solutions are passing some rudimentary tests.

  To run the test suite, simply type following into your console (you will have to
  exit the repl first, or open another console window):

  >> cabal new-test

  If this fails due to missing hspec, you will have to install it:

  >> cabal new-install hspec

  This will run the tests and print out any errors you may have. Also make sure to
  uncomment tests for extra tasks if you are solving them.

  __YOU CAN ADD YOUR OWN TESTS BUT DO NOT MODIFY OR REMOVE ANY PRE-EXISTING TESTS!__
  __THERE WILL BE SANCTIONS FOR SUCH ACTIONS!__

  PROTIP: You can convert this whole file into a more readable HTML document by
  running the following command:

  >> cabal new-haddock

  And opening the resulting file (you will get the location in the console) with your
  browser.
-}
-- =============================================================================== --
--
module TrainingExercises where
--
import Data.List
import Data.Char
--

{- * 1.1 IF-THEN-ELSE & GUARDS -}

-- ** TE 1.1.1
--
-- | Using IF-THEN-ELSE create a function which takes in an 'Int' and divides it by 2
-- if it's even otherwise it adds 1 to it and than divides it by 2.
--
-- You can use 'even' and 'div' functions to make things easier.
te111 :: Integral a => a -> a
te111 x = if even x  then x `div` 2 else (x + 1) `div` 2

-- ** TE 1.1.2
--
-- | Using GUARDS, implement a function which takes in an 'Int' and for numbers from
-- 1 to 3 it returns "one", "two" and "three" respectively, for everything else
-- return "out of range".
te112 :: (Eq a, Num a) => a -> [Char]
te112 x 
  | x == 1 = "one"
  | x == 2 = "two"
  | x == 3 = "three"
  | otherwise = "out of range"

-- ** TE 1.1.3
--
-- | Given the following three ranges (not lists) [1,5), [-4,0] and (25,100] by using
-- GUARDS implement a function which takes in an 'Int', checks if the number is
-- within any of the three ranges. If it is, then print out that range, otherwise
-- return "out of range".
--
-- Message should be in the following format: "number is in the [1,5) range"
te113 :: (Ord a, Num a) => a -> [Char]
te113 x 
  | (x > 0) && (x < 5) = "number is in the [1,5) range"
  | (x > -5) && (x < 1) = "number is in the [-4,0] range"
  | (x > 25) && (x < 101) = "number is in the (25,100] range"
  | otherwise = "out of range"
--

{- * 1.2 LIST & STRINGS -}

-- ** TE 1.2.1
--
-- | Implement a function which takes in two lists of the same type and returns the
-- longer one. If the lists are of equal length concatenate them and return that.
te121 :: [a] -> [a] -> [a]
te121 l1 l2
  | length l1 > length l2 = l1
  | length l2 > length l1 = l2
  | length l2 == length l1 = l1 ++ l2

-- ** TE 1.2.2
--
-- | Implement a function which adds '.' to the end of the 'String' by using ':'
-- (cons operator) and 'reverse' function.
te122 :: [Char] -> [Char]
te122 x = reverse('.':(reverse x))

-- ** TE 1.2.3
--
-- | Implement a function which removes first three and last three words from the
-- 'String'. If there are not enough words, than return the empty 'String'.
--
-- You can use functions 'words' and 'unwords' to split the 'String' into a list of
-- words.

wordlen :: String -> Int
wordlen x = length $ words x
kill3 :: [a] -> [a]
kill3 x = drop 3 $ take (length x - 3) x
te123 :: [Char] -> [Char]
te123 x =  if wordlen x >= 6 then unwords $ kill3 $ words x else ""

--

{- * 1.3 TUPLES & LIST COMPREHENSIONS -}

-- ** TE 1.3.1
--
-- | Using LIST COMPREHENSIONS and TOUPLES implement a function which will take a
-- starting number 'n' and construct infinite list of pairs of numbers and their
-- squares, where numbers are going from 'n' to the infinity in the increments of
-- two.
--
-- Here is an example of such list where 'n' is 3:
-- [(3,9),(5,25),(7,49),(9,81),...]
te131 :: (Num b, Enum b) => b -> [(b, b)]
te131 n = [(x,x*x) | x <- [n,n+2..]]

-- ** TE 1.3.2
--
-- | Given the lists of 'adjective's and 'noun's, by using LIST COMPREHENSIONS
-- generate a list of all possible book titles in the form of:
--
-- "The {adjective} {noun}"
--
-- and add index to them.
--
-- Example of a single indexed book title in a tuple:
--
-- (2, "The Deadly Bicycle")
--
-- Use the 'adjective' and 'noun' lists in your list comprehension
-- (don't modify them). 'te132' should not take any arguments.
--
-- Also, pick one title and write a short story of 250 words max. (jk. lol :)
index :: (Num a, Enum a) => [b] -> [(a, b)]
index xs = zip [0..] xs
te132 :: [(Integer, [Char])]
te132 = zip [1..] [ "The " ++ a ++ " " ++ n | a <- adjective, n <- noun]

-- | Lists which you should use in your 'te132' implementation.
adjective, noun :: [String]
( adjective, noun ) =
  ( ["Whispering", "Incredible", "Wild", "Deadly"]
  , ["Forest", "Bicycle", "River", "Woman", "Necklace"]
  )

-- ** TE 1.3.3
--
-- | Implement a function which takes in a list of pairs / tuples of 'Int's and
-- returns a list of their sums.
te133 :: Num a => [(a, a)] -> [a]
te133 p = [x + y | (x, y) <- p]

