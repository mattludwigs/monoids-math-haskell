module Code where

import Control.Applicative
{-

  class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

  :t <$> :: (a -> b) -> f a -> f b
  :t <*> :: f (a -> b) -> f a -> f b

  import Control.Applicative

  :t liftA :: Applicative f => (a -> b) -> f a -> f b
  :t <$>   :: Functor F     => (a -> b) -> f a -> f b
-}

f x = lookup x [(3, "Hello"), (4, "Julie"), (5, "kbai")]
g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]

h z = lookup z [(2, 3), (5, 6), (7, 8)]
m x = lookup x [(4, 10), (8, 13), (1, 9001)]


added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z


newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)


validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen then
    Nothing
  else
    Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person
  = Person Name Address
  deriving (Eq, Show)

{- Before Applicative
-- mkPerson :: String -> String -> Maybe Person
-- mkPerson n a =
--   case mkName n of
--     Nothing -> Nothing
--     Just n' ->
--       case mkAddress a of
--         Nothing -> Nothing
--         Just a' ->
--           Just $ Person n' a'
-}

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  Person <$> mkName n <*> mkAddress a




data Cow = Cow
  { name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing


-- cowFromString :: String -> Int -> Int -> Maybe Cow
-- cowFromString name' age' weight' =
--   case noEmpty name' of
--     Nothing -> Nothing
--     Just nammy ->
--       case noNegative age' of
--         Nothing -> Nothing
--         Just agey ->
--           case noNegative weight' of
--             Nothing -> Nothing
--             Just weighty ->
--               Just (Cow nammy agey weighty)


cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name age weight =
  Cow <$> noEmpty name
      <*> noNegative age
      <*> noNegative weight

cowFromString' :: String -> Int -> Int -> Maybe Cow
cowFromString' name age weight =
  liftA3 Cow
          (noEmpty name)
          (noNegative age)
          (noNegative weight)


data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Show)


instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)


instance Monoid e => Applicative (Validation e) where
  pure = Success
  Failure e <*> Failure e' = Failure (e <> e')
  Failure e <*> Success _ = Failure e
  Success _ <*> Failure e = Failure e
  Success f <*> Success a = Success (f a)


data Pair a
  = Pair a a
  deriving (Show)


instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure a = Pair a a
  Pair f f' <*> Pair a a' = Pair (f a) (f' a')


data Two a b
  = Two a b
  deriving Show

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure = Two memepty
  Two a f <*> Two a' x = Two (a <> a') (f x)
