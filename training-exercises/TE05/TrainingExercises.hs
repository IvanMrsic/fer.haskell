-- =============================================================================== --
{- |
  Welcome to your fifth Haskell training. Get ready to rumble.
  Where will you let recursion lead you?

  You should know the drill by now - solve the exercises in `TrainingExercises.hs`,
  run the tests with Cabal, push to `training-05`, create a Merge Request,
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

{- * 5.1 Recursive functions with accumulators -}

{-
 - For the following functions, we'd like you to to use the `seq` function to
 - reduce the accumulators to WHNF where needed to avoid building big thunks.
 -}

-- ** TE 5.1.1
--
-- | Define a recursive function with accumulation which finds the
-- | first longest word in a sentence.
-- | Make sure you keep track of the length of the currently longest word
-- | too, so you don't call `length` repeatedly on the same word.

te511 :: String -> String
te511 xs = longest sentence 0 ""
  where 
    sentence         = words xs
    longest [] _ n   = n
    longest (x:xs) l n 
      | length x > l = longest xs (length x) x 
      | otherwise    = longest xs l n 


-- ** TE 5.1.2
--
-- | Define a recursive function with accumulation which takes a list of
-- | polynomial coefficients and a variable, and calculates the polynomial using
-- | Horner's method (https://en.wikipedia.org/wiki/Horner%27s_method ).
-- | The coefficients are in descending order of variable exponents, so a list
-- | representing the polynomial x^3 - 2x + 3 would be  as  [1, 0, -2, 3].

te512 :: Num a => [a] -> a -> a
te512 xs x = foldr (\a b -> a + b*x) 0 (reverse xs)


-- ** TE 5.1.3
--
-- | Define a function which computes the population standard deviation of a list of
-- | numbers. To achieve this you need to compute the mean and variance of the list:
-- | do this using recursive functions with accumulation.

te513 :: Floating a => [a] -> a
te513 [] = error "smth"
te513 xs = sqrt . average . map ((^2) . (-) axs) $ xs
          where average = (/) <$> sum <*> realToFrac . length
                axs     = average xs

-- ** TE 5.1.4
--
-- | An aspiring rollercoaster designer wants to test out his if his newest
-- | creation is safe to ride, and needs your help!
-- | Define a function which takes a list of pairs which describe a section of
-- | the track. The first element will be a String which will be either "even",
-- | "drop", "turn left" or "turn right". The second element will be a number.
-- |
-- | If it's a "drop", the car accelerates as if it was in free-fall (accelerating
-- | 9.81 m/s^2) and the number indicates the height of the drop in meters.
-- | The car maintains its current speed coming into the drop.
-- |
-- | If it's "even", the car decelerates by 0.5 m/s every meter it passes. The
-- | number indicates the length of the even segment. If the car decelerates to
-- | 0 km/s, the track is deemed unsafe as the passengers will become stuck!
-- |
-- | If it's either of the two "turn"s, the number indicates the radius of the
-- | turn in meters. The car will derail if it turns too tightly: it can only
-- | withstand centripetal acceleration of up to and including 5G. And if there
-- | are 3 or more alternating turns directly in a row, the passengers will become
-- | nauseous, which can be unsafe.
-- |
-- | The car starts moving at 20 km/h. The function must return a list indicating
-- | whether the rollercoaster is safe or not. If it is safe, it returns an empty list.
-- | If the rollercoaster is not safe, it returns a list with one element: the
-- | index of the segment of the track where it becomes unsafe.
-- | (Later on, you will learn a much more elegant way of representing a result
-- | which might contain a value, or might contain nothing at all, but this will
-- | do for now.)


fall speed len = speed + 9.81 * t
  where t = ((-speed) + sqrt (speed * speed + 2 * 9.81 * len)) / 9.81

acceleration speed r = speed * speed / r

te514 :: [(String, Double)] -> [Int]
te514 [] = error "erorr"
te514 track = rollercoaster track 0 (20 * 1000 / 3600) "" 0
  where
    rollercoaster [] _ _ _ _ = []
    rollercoaster (("drop", len):ss) ix speed _ _ =
      let speed1 = fall speed len in speed1 `seq` rollercoaster ss (ix + 1) speed1 "" 0
    rollercoaster (("even", len):ss) ix speed _ _
      | speed1 <= 0 = [ix]
      | otherwise = rollercoaster ss (ix + 1) speed1 "" 0
      where speed1 = speed - len * 0.5
    rollercoaster ((turn, r):ss) ix speed lastdir turns
      | speed * speed / r > 9.81 * 5         = [ix] 
      | lastdir == "" || lastdir == turn = rollercoaster ss (ix + 1) speed turn 1
      | turns < 2                        = rollercoaster ss (ix + 1) speed turn (succ turns)
      | otherwise                        = [ix] 
