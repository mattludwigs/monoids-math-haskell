module Code where

import Control.Monad (join)

{-

  class Applicative m => Monad (m :: * -> *) where
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    return :: a -> m a
    fail :: String -> m a
    {-# MINIMAL (>>=) #-}

  fmap xs = xs >>= return . (+1)


  fmap :: Functor f   =>   (a -> b) -> f a        -> f b
  <*>  :: Applicative => f (a -> b) -> f a        -> f b
  >>=  :: Monad f     => f a        -> (a -> f b) -> f b


  fmap :: Functor f => (a -> f b) -> f a -> f (f b)

  let andOne x = [x, 1]
  andOne 10
  [10, 1]

  fmap andOne [4, 5, 6]
  [[4, 1], [5, 1], [6, 1]]


  ---- WE NOW HAVE EXTRA STRUCTURE ----

  some foldable with a list of `a`
  concat :: Foldable t => t [a] -> [a]

  removes outter structure from a list

  [[4, 1], [5, 1], [6, 1]]

  concat :: [[a]] -> [a]

  `Control.Monad` has  generalization of `concat`

  join :: Monad m => m (m a) -> m a

-}



{-
  implement bind in terms of `fmap` and `join`
-}


bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x


bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)


bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls" >>
  getLine >>=
    \name -> putStrLn ("y helo thar: " ++ name)


twoBindings :: IO ()
twoBindings = do
  putStrLn "name pls: "
  name <- getLine
  putStrLn "age pls: "
  age <- getLine
  putStrLn (mconcat ["y helo thar: ", name, " who is: ", age, " years old"])


twoBindings' :: IO ()
twoBindings' =
  putStrLn "name pls: " >>
  getLine >>=
    \name ->
      putStrLn "age pls: " >>
      getLine >>=
        \age ->
          putStrLn (mconcat ["y helo thar: ", name, " who is: ", age, " years old"])


{-

  Monads in code


  (>>=) :: Monad m =>  m a -> (a ->  m b) ->  m b
  (>>=) ::            [] a -> (a -> [] b) -> [] b

  (>>=) ::            [a] -> (a -> [b]) -> [b]

  return :: Monad m => a ->  m a
  return ::            a -> [] a
  return ::            a -> [a]
-}


twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x then
    [x * x, x * x]
  else
    [x * x]


twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x then
    [x * x, x * x]
  else
    []

{-

  Maybe Monad

  (>>=) :: Monad m => m a -> (a ->     m b) ->     m b
  (>>=) ::        Maybe a -> (a -> Maybe b) -> Maybe b

  return :: Monad m => a ->     m a
  return ∷       Maybe a -> Maybe a

-}

data Cow = Cow
  { name :: String
  , age :: Int
  , weight :: Int
  } deriving (Show, Eq)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing


weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let
    w = weight c
    n = name c
  in
    if n == "Bess" && w > 499 then
      Nothing
    else
      Just c


-- mkSphericalCow :: String -> Int -> Int -> Maybe Cow
-- mkSphericalCow name' age' weight' =
--   case noEmpty name' of
--     Nothing -> Nothing
--     Just nammy ->
--       case noNegative age' of
--         Nothing -> Nothing
--         Just agey ->
--           case noNegative weight' of
--             Nothing -> Nothing
--             Just weighty ->
--               weightCheck (Cow nammy agey weighty)


mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)



{-
  Either

  (>>=) :: Monad m => m a -> (a ->        m b) ->        m b
  (>>=) ::     Either e a -> (a -> Either e b) -> Either e b


  return :: Monad m => a -> m a
  return ::            a -> Either e a

-}


type Founded = Int
type Coders = Int

data SoftwareShop =
  Shop
    { founded :: Founded
    , programmers :: Coders
    } deriving (Eq, Show)


data FoundedError
  = NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0 = Left $ NegativeYears n
  | n > 500 = Left $ TooManyYears n
  | otherwise = Right n


validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0 = Left $ NegativeCoders n
  | n > 5000 = Left $ TooManyCoders n
  | otherwise = Right n


mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  programmers <- validateCoders coders

  if programmers > div founded 10 then
    Left $ TooManyCodersForYears founded programmers
  else
    Right $ Shop founded programmers



data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)


instance Applicative (Sum a) where
  pure = Second
  (First a) <*> _ = First a
  _ <*> (First a) = First a
  (Second f) <*> (Second a) = Second (f a)


instance Monad (Sum a) where
  return = pure
  (>>=) (First a) _ = First a
  (>>=) (Second a) f = f a


data Nope a
  = NopeDogJpg
  deriving (Show)


instance Functor Nope where
  fmap f NopeDogJpg = NopeDogJpg


instance Applicative Nope where
  pure x = NopeDogJpg
  (<*>) _ _ = NopeDogJpg


instance Monad Nope where
  return = pure
  NopeDogJpg >>= f = NopeDogJpg


newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a


instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a


instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a


addStuff :: Int -> Int
addStuff = do
  n <- (+1)
  return (n + n)
