module Exercises where

class PopQuiz a

-- | Which of the following instances require 'FlexibleInstances'? Don't cheat
-- :D This is a tricky one, but look out for nested concrete types!

-- instance PopQuiz Bool
-- instance PopQuiz [Bool] -- | this
-- instance PopQuiz [a]
-- instance PopQuiz (a, b)
-- instance PopQuiz [(a, b)] -- | this
-- instance PopQuiz (IO a)

newtype RIO  r a = RIO (r -> IO a) -- Remember, this is a /new type/.
type    RIO' r a =      r -> IO a

-- instance PopQuiz (RIO Int a) -- | this
-- instance PopQuiz (RIO r a)
-- instance PopQuiz (RIO' r a) -- | oops, missed this
-- instance PopQuiz (r -> IO a) -- | this
-- instance PopQuiz (a -> b) -- We can write (a -> b) as ((->) a b).
-- instance PopQuiz (a -> b -> c) -- | oops, missed this
-- instance PopQuiz (a, b, c)
-- instance PopQuiz (a, (b, c)) -- | this
-- instance PopQuiz ()
-- instance PopQuiz (a, b, c, a) -- | oops, missed uniqueness

data Pair  a = Pair  a  a
type Pair' a =      (a, a)

-- instance PopQuiz (a, a) -- | this
-- instance PopQuiz (Pair a)
-- instance PopQuiz (Pair' a) -- | this
