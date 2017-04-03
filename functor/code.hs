module Code where

{-
  :kind Two : * -> * -> *
-}
data Two a b =
  Two a b
  deriving (Eq, Show)

data Or a b =
    First a
  | Second b
  deriving (Eq, Show)


{-
  Cannot make a Functor instance like so:

  instance Functor Two where
    fmap = undefined


  instance Functor Or where
    fmap = undefined


  namely becuase a Functor must have a kind of `* -> *`, and these
  have a kind of `* -> * -> *`

  But we can partially apply function the functions to get the type we
  want.
-}

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)


instance Functor (Or a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)


newtype Identity a =
  Identity a
  deriving (Show)


instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

data Pair a
  = Pair a a
  deriving (Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)


data Three a b c
  = Three a b c
  deriving (Show)


instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)



data Three' a b
  = Three' a b b
  deriving Show

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')


{-
  Following two functions follow the same pattern

  That is to say they both are function that just returns `Nothing` if `Nothing` is passed in

  And they call some function on the `Just` value if a `Just` was passed in
-}

incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust Nothing = Nothing


showIfJust :: Show a => Maybe a -> Maybe String
showIfJust (Just s) = Just $ show s
showIfJust Nothing = Nothing


{-
  The next two functions will reduce the redundancy using fmap
-}

incMaybe' :: Num a => Maybe a -> Maybe a
incMaybe' m = fmap (+1) m

showIfJust' :: Show a => Maybe a -> Maybe String
showIfJust' m = fmap show m


{-
  We can even remove the arguments and use point free style
-}

incMaybe'' :: Num a => Maybe a -> Maybe a
incMaybe'' = fmap (+1)

showMaybe'' :: Show a => Maybe a -> Maybe String
showMaybe'' = fmap show

{-
  Since `fmap` can work on more than just the `Maybe` type, we can abstract further to use it
  on all Functors
-}

liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show


data Possibly a
  = LolNope
  | Yeppers a
  deriving (Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers $ f a


applyIfYep :: (a -> b) -> Possibly a -> Possibly b
applyIfYep f = fmap f


{-
  Looking at `Either` the same way we looked at `Maybe`

  Again the two functions have similar struture in that the error case goes "untouched" and
  there is some function that the inner most value is getting applied to
-}

incIfRight :: Num a => Either e a -> Either e a
incIfRight (Right n) = Right $ n + 1
incIfRight (Left e) = Left e

showIfRight :: Show a => Either e a -> Either e String
showIfRight (Right s) = Right $ show s
showIfRight (Left e) = Left e

{-
  Same as above only we remove the need to explictly account for the ignoring of the error case
-}
incEither :: Num a => Either e a -> Either e a
incEither e = fmap (+1) e

showEither :: Show a => Either e a -> Either e String
showEither e = fmap show e


{-
  Again since `Either` is a `Functor` we can the liftedInc and liftedShow on `Either` to get
  the desired output
-}

data Sum a b
  = First' a
  | Second' b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First' a) = First' a
  fmap f (Second' a) = Second' $ f a


applyIfSecond :: (a -> b) -> (Sum e) a -> (Sum e) b
applyIfSecond f e = fmap f e


{-
  Constant Functor
-}

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show)


instance Functor (Constant m) where
  fmap _ (Constant v) = Constant v



data Wrap f a
  = Wrap (f a)
  deriving (Eq, Show)


instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)


{-
  Functor below is a bad functor because it breaks the law of composition.
-}

data Bad a
  = BadFunctor Integer a
  deriving (Show)


instance Functor Bad where
  fmap f (BadFunctor n a) = BadFunctor (n + 1) (f a)
