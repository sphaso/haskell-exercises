{-# LANGUAGE GADTs #-}
module Exercises where





{- ONE -}

-- | Let's introduce a new class, 'Countable', and some instances to match.
class Countable a where count :: a -> Int
instance Countable Int  where count   = id
instance Countable [a]  where count   = length
instance Countable Bool where count x = if x then 1 else 0

-- | a. Build a GADT, 'CountableList', that can hold a list of 'Countable'
-- things.

data CountableList where
  CountANil :: CountableList
  CountACons :: Countable a => a -> CountableList -> CountableList


-- | b. Write a function that takes the sum of all members of a 'CountableList'
-- once they have been 'count'ed.

countList :: CountableList -> Int
countList CountANil = 0
countList (CountACons h t) = (count h) + (countList t)


-- | c. Write a function that removes all elements whose count is 0.

dropZero :: CountableList -> CountableList
dropZero CountANil = CountANil
dropZero (CountACons h t) | count h == 0 = dropZero t
                          | otherwise    = CountACons h (dropZero t)


-- | d. Can we write a function that removes all the things in the list of type
-- 'Int'? If not, why not?

-- | Sphaso: how do I check for a type without ScopedTypeVariables?
filterInts :: CountableList -> CountableList
filterInts CountANil = error "Contemplate me!"





{- TWO -}

-- | a. Write a list that can take /any/ type, without any constraints.

data AnyList where
  AnyNil :: AnyList
  AnyCons :: a -> AnyList -> AnyList

-- | b. How many of the following functions can we implement for an 'AnyList'?

reverseAnyList :: AnyList -> AnyList
reverseAnyList AnyNil = AnyNil
reverseAnyList (AnyCons h t) = AnyCons (reverseAnyList t) (AnyCons h AnyNil)

-- | Sphaso: only assuming `a` is fixed for all values in the AnyList
filterAnyList :: (a -> Bool) -> AnyList -> AnyList
filterAnyList = undefined

lengthAnyList :: AnyList -> Int
lengthAnyList AnyNil = 0
lengthAnyList (AnyCons _ t) = 1 + lengthAnyList t

-- | Sphaso: err... AnyList does not implement Foldable, nor its members
-- implement Monoid. I don't think this is doable
foldAnyList :: Monoid m => AnyList -> m
foldAnyList = undefined

isEmptyAnyList :: AnyList -> Bool
isEmptyAnyList AnyNil = True
isEmptyAnyList _      = False

-- | Sphaso: nope. `a` in the GADT is not constrained by Show
instance Show AnyList where
  show = error "What about me?"





{- THREE -}

-- | Consider the following GADT:

data TransformableTo output where
  TransformWith
    :: (input -> output)
    ->  input
    -> TransformableTo output

-- | ... and the following values of this GADT:

transformable1 :: TransformableTo String
transformable1 = TransformWith show 2.5

transformable2 :: TransformableTo String
transformable2 = TransformWith (uncurry (++)) ("Hello,", " world!")

-- | a. Which type variable is existential inside 'TransformableTo'? What is
-- the only thing we can do to it?

-- | Sphaso: the existential variable is input, we can only apply it to the
-- first argument (the function)

-- | b. Could we write an 'Eq' instance for 'TransformableTo'? What would we be
-- able to check?

-- | Sphaso: I don't think so, output is not constrained by Eq

-- | c. Could we write a 'Functor' instance for 'TransformableTo'? If so, write
-- it. If not, why not?

instance Functor TransformableTo where
  fmap g (TransformWith f x) = TransformWith (g . f) x


{- FOUR -}

-- | Here's another GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- | a. There's one (maybe two) useful function to write for 'EqPair'; what is
-- it?

-- | Sphaso: we could see if the elements are the same

-- | b. How could we change the type so that @a@ is not existential? (Don't
-- overthink it!)

-- | Sphaso: remove the constraint, for what it's worth
-- | Answer: NOPE, just move Eq up

-- | c. If we made the change that was suggested in (b), would we still need a
-- GADT? Or could we now represent our type as an ADT?

-- | Sphaso: we wouldn't need a GADT since there's not constraint
-- | Answer: we wouldn't need it, but we lose the constraint



{- FIVE -}

-- | Perhaps a slightly less intuitive feature of GADTs is that we can set our
-- type parameters (in this case @a@) to different types depending on the
-- constructor.

data MysteryBox a where
  EmptyBox  ::                                MysteryBox ()
  IntBox    :: Int    -> MysteryBox ()     -> MysteryBox Int
  StringBox :: String -> MysteryBox Int    -> MysteryBox String
  BoolBox   :: Bool   -> MysteryBox String -> MysteryBox Bool

-- | When we pattern-match, the type-checker is clever enough to
-- restrict the branches we have to check to the ones that could produce
-- something of the given type.

getInt :: MysteryBox Int -> Int
getInt (IntBox int _) = int

-- | a. Implement the following function by returning a value directly from a
-- pattern-match:

getInt' :: MysteryBox String -> Int
getInt' (StringBox _ (IntBox n _)) = n

-- | b. Write the following function. Again, don't overthink it!

countLayers :: MysteryBox a -> Int
countLayers EmptyBox        = 0
countLayers (IntBox _ t)    = 1 + countLayers t
countLayers (StringBox _ t) = 1 + countLayers t
countLayers (BoolBox _ t)   = 1 + countLayers t

-- | c. Try to implement a function that removes one layer of "Box". For
-- example, this should turn a BoolBox into a StringBox, and so on. What gets
-- in our way? What would its type be?

-- | Sphaso:
-- removeLayer :: MysteryBox a -> MysteryBox b
-- removeLayer (BoolBox _ i) = i

-- Could not deduce: b ~ String
-- from the context: a ~ Bool
--  bound by a pattern with constructor:
--             BoolBox :: Bool -> MysteryBox String -> MysteryBox Bool,
--           in an equation for ‘removeLayer’
--
-- | there's not way to generalize the output type `b`


{- SIX -}

-- | We can even use our type parameters to keep track of the types inside an
-- 'HList'!  For example, this heterogeneous list contains no existentials:

data HList a where
  HNil  :: HList ()
  HCons :: head -> HList tail -> HList (head, tail)

exampleHList :: HList (String, (Int, (Bool, ())))
exampleHList = HCons "Tom" (HCons 25 (HCons True HNil))

-- | a. Write a 'head' function for this 'HList' type. This head function
-- should be /safe/: you can use the type signature to tell GHC that you won't
-- need to pattern-match on HNil, and therefore the return type shouldn't be
-- wrapped in a 'Maybe'!

head' :: HList (h, t) -> h
head' (HCons h _) = h

-- | b. Currently, the tuples are nested. Can you pattern-match on something of
-- type @HList (Int, String, Bool, ())@? Which constructor would work?

-- | Sphaso: absolutely no idea on how to pattern match on a 4-tuple,
-- HList only has a 2-tuple...

patternMatchMe :: HList (Int, String, Bool, ()) -> Int
patternMatchMe = undefined

-- | c. Can you write a function that appends one 'HList' to the end of
-- another? What problems do you run into?

-- | Sphaso: I think the usual problem with `a` being too generic would
-- arise


{- SEVEN -}

-- | Here are two data types that may help:

data Empty
data Branch left centre right

-- | a. Using these, and the outline for 'HList' above, build a heterogeneous
-- /tree/. None of the variables should be existential.

data HTree a where
  HEmpty :: HTree Empty
  HBranch :: HTree l -> c -> HTree r -> HTree (Branch l c r)

-- | b. Implement a function that deletes the left subtree. The type should be
-- strong enough that GHC will do most of the work for you. Once you have it,
-- try breaking the implementation - does it type-check? If not, why not?

deleteLeft :: HTree (Branch l c r) -> HTree (Branch Empty c r)
deleteLeft (HBranch _ c r) = HBranch HEmpty c r

-- | c. Implement 'Eq' for 'HTree's. Note that you might have to write more
-- than one to cover all possible HTrees. You might also need an extension or
-- two, so look out for something... flexible... in the error messages!
-- Recursion is your friend here - you shouldn't need to add a constraint to
-- the GADT!

--instance Eq a => Eq (HTree a) where
--    (==) HEmpty HEmpty = True
--    (==) (HBranch l c r) (HBranch l' c' r') = (l == l') && (c == c') && (r == r')
--    (==) _ _ = False

-- | Sphaso: this does not work, I had to peek at the solution. Such
-- a shame


{- EIGHT -}

-- | a. Implement the following GADT such that values of this type are lists of
-- values alternating between the two types. For example:
--
-- @
--   f :: AlternatingList Bool Int
--   f = ACons True (ACons 1 (ACons False (ACons 2 ANil)))
-- @

data AlternatingList a b where
  ANil :: AlternatingList a b
  ACons :: a -> AlternatingList b a -> AlternatingList a b

-- | b. Implement the following functions.

getFirsts :: AlternatingList a b -> [a]
getFirsts ANil                  = []
getFirsts (ACons a ANil)        = [a]
getFirsts (ACons a (ACons _ b)) = a : getFirsts b

getSeconds :: AlternatingList a b -> [b]
getSeconds ANil                  = []
getSeconds (ACons _ ANil)        = []
getSeconds (ACons _ (ACons a b)) = a : getSeconds b

-- | c. One more for luck: write this one using the above two functions, and
-- then write it such that it only does a single pass over the list.

foldValues :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues l = (mconcat $ getFirsts l, mconcat $ getSeconds l)




{- NINE -}

-- | Here's the "classic" example of a GADT, in which we build a simple
-- expression language. Note that we use the type parameter to make sure that
-- our expression is well-formed.

data Expr a where
  Equals    :: Expr Int  -> Expr Int            -> Expr Bool
  Add       :: Expr Int  -> Expr Int            -> Expr Int
  If        :: Expr Bool -> Expr a   -> Expr a  -> Expr a
  IntValue  :: Int                              -> Expr Int
  BoolValue :: Bool                             -> Expr Bool

-- | a. Implement the following function and marvel at the typechecker:

eval :: Expr a -> a
eval (IntValue n) = n
eval (BoolValue b) = b
eval (Equals a b) = eval a == eval b
eval (Add a b) = eval a + eval b
eval (If c t f) | eval c = eval t
                | otherwise = eval f

-- | b. Here's an "untyped" expression language. Implement a parser from this
-- into our well-typed language. Note that (until we cover higher-rank
-- polymorphism) we have to fix the return type. Why do you think this is?

data DirtyExpr
  = DirtyEquals    DirtyExpr DirtyExpr
  | DirtyAdd       DirtyExpr DirtyExpr
  | DirtyIf        DirtyExpr DirtyExpr DirtyExpr
  | DirtyIntValue  Int
  | DirtyBoolValue Bool

parse :: DirtyExpr -> Maybe (Expr Int)
parse (DirtyIntValue n) = error "Implement me"

-- | Sphaso: I looked at the solution and all I can say is... oh boy!

-- | c. Can we add functions to our 'Expr' language? If not, why not? What
-- other constructs would we need to add? Could we still avoid 'Maybe' in the
-- 'eval' function?


-- | Sphaso: You can


{- TEN -}

-- | Back in the glory days when I wrote JavaScript, I could make a composition
-- list like @pipe([f, g, h, i, j])@, and it would pass a value from the left
-- side of the list to the right. In Haskell, I can't do that, because the
-- functions all have to have the same type :(

-- | a. Fix that for me - write a list that allows me to hold any functions as
-- long as the input of one lines up with the output of the next.

data TypeAlignedList a b where
  TypedNil :: TypeAlignedList a b
  TypedCons :: (a -> b) -> TypeAlignedList b a -> TypeAlignedList a b


-- | b. Which types are existential?

-- | Sphaso: none

-- | c. Write a function to append type-aligned lists. This is almost certainly
-- not as difficult as you'd initially think.

composeTALs :: TypeAlignedList b c -> TypeAlignedList a b -> TypeAlignedList a c
composeTALs (TypedCons f x) (TypedCons g y) = TypedCons (f . g) (composeTALs y x)
