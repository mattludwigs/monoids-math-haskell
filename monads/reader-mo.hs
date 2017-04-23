{-# LANGUAGE InstanceSigs #-}
module ReadMo where

import Control.Applicative

boop = (*2)
doop = (+10)


bip :: Integer -> Integer
bip = boop . doop

{-
  What is the functorial context?

  fmap (+1) [1, 2, 3]

  [ (1 + 1), (2 + 1), (3 + 1) ]

  fmap boop doop x = (*2)((+10) x)
-}
bloop :: Integer -> Integer
bloop = fmap boop doop



{-
  ((+) <$> (*2) <*> (+10)) 3

  (*2) :: Num a => a -> a
  (+)  :: Num a => a -> a -> a
  (+) <$> (*2) :: Num a -> a -> a


  (<*>) :: Applicative f =>  f (a -> b) -> f a -> f b
  f = ((-> a))

  (<*>) :: ((-> a) a -> b) -> ((-> a) a) -> ((-> a) b)
  (<*>) :: (a -> a -> b) -> (a -> a) -> (a -> b)

  ((+) <$> (*2) <*> (+10)) 3

  ((+) <$> (3 * 2) <*> (3 + 10))
  (+) 6 13
  19
-}
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

-- doopDoop :: Integer -> Integer
-- boopDoop = do
--   a <- boop
--   b <- doop
--   return (a + b)


newtype Reader r a =
  Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id


newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)


data Person = Person
  { humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)


data Dog = Dog
  { dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)


pers :: Person
pers =
  Person (HumanName "Big Bird")
         (DogName "Barkley")
         (Address "Sesame Street")

chris :: Person
chris =
  Person (HumanName "Chris Allen")
         (DogName "Papu")
         (Address "Austin")

-- without Reader
getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)


getDogR' :: Person -> Dog
getDogR' =
  Dog <$> dogName <*> address

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b


asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra


instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  Reader f <*> Reader g =
      Reader (\r -> f r (g r))


foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)


barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length Int)
