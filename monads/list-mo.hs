module MTrans where

{-
  Explorations in monad transformers and list monads
-}

{-
  List monad:

  return x = [x]

  > return 1 :: [Int]
  1
  > return 3 :: [Int]
  3

  The interior objects of a list are just the elements of the list

  (>>=) :: Monad m => m a  -> (a -> m b) -> m b
  (>>=)               list -> f          -> (concat (map f m))


  where `f` returns a new `m`

  (\x -> [x + 1])

  myMapper :: a   -> m b
  myMapper :: Int -> [Int]

  myMapper = \x -> [x + 1]

  > map myMapper [1..5]
  [[2],[3],[4],[5],[6]]

  The we call `concat` on that output

  > concat [[2],[3],[4],[5],[6]]
  [2,3,4,5,6]

  > [1..5] >>= myMapper
  [2,3,4,5,6]
-}

toList :: a -> [a]
toList x = return x

toList' :: a -> [a]
toList' = return

{-
  > toList 1
  [1]
  > toList' 1
  [1]
  > (toList' . (+1)) 1
  [2]
-}


listInDo = do
  -- The monadic context for the return is a List so `return (x + 1)` is like saying `[x + 1]`
  [1,2] >>= (\x -> return (x + 1))

{-
  Applying rule is to says

  monad >>= someFunc

  val <- monad
  return (someFunc val)
-}
listDoWithRule2 :: [Int]
listDoWithRule2 = do
  x <- [1, 2]
  return (x + 1)

{-
  Pairs

  > paris [1, 2] ['a', 'b']
  [(1, 'a'), (1, 'b'), (2, 'a'), (2, 'b')]
-}

pairs :: [a] -> [b] -> [(a, b)]
pairs l1 l2 = do
  x <- l1
  do
    y <- l2
    return (x, y) -- This is the same as (\(x y) -> return (x, y))

pairs' :: [a] -> [b] -> [(a, b)]
pairs' l1 l2 = do
  x <- l1
  do
    l2 >>= (\y -> return (x, y)) -- do mon >>= f is the same as mon >>= f

pairs'' :: [a] -> [b] -> [(a, b)]
pairs'' l1 l2 = do
  x <- l1
  l2 >>= (\y -> return (x, y)) -- mon >>= f is the same as do x <- mon, f x

pairs''' :: [a] -> [b] -> [(a, b)]
pairs''' l1 l2 = do
  x <- l1
  y <- l2
  return (x, y)


{-
  A more generlized type signature of `pairs` is:

  pairs :: Monad m => m a -> m b -> m (a, b)

  Lets specialize it:

  pairs ::            [a] -> [b]        -> [(a, b)]
  paris ::        Maybe a -> Maybe b    ->  Maybe (a, b)
  pairs ::          IO a  -> IO b       -> IO (a, b)
  pairs ∷      Either e a -> Either e a -> Either e (a, b)

  The output of `pairs getLine getLine`

  > :t getLine
  IO String

  pairs :: IO String -> IO String -> IO (String, String)

  > pairs getLine getLine
  hello
  world
  ("hello", "world")
-}

pairs''''' :: Monad m => m a -> m b -> m (a, b)
pairs''''' m1 m2 = do
  x <- m1
  y <- m2
  return (x, y)
