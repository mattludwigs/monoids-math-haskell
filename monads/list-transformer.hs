module ListTransformer where

import Control.Monad.List

-- Adventures into trying to understand Monads

{-
  Mixing Monads


  This wont work:

  wrong = do
    x <- [1, 2] -- [Int]
    y <- ["a", "b"] -- [String]
    putStrLn (show (x, y)) -- IO ()

  This is because we are mixing the List monad and the IO monad. The type cannot
  be `wrong :: [Int] | IO ()`

  A transformer is not a monad but a constructor of monads

  newtype ListT m a =
    ListT { runListT :: m [a] }

  Give ListT some monad `m`, like IO or Maybe, and now that monad transformer will become a Monad. When `m` and `a`
  are appied we get a new monadic object that will work like the list monad in regards to `return` and `>>=`.


  This  will not work:

  hello :: ListT IO ()
  hello = do
    putStrLn "This is wrong!"

  The reason is because `putStrLn` type is `IO ()` not `ListT IO ()`


  There is `lift` function to solve that

  lift :: (MonadTrans t, Monad m) => m a       -> t m a
  lift ::                            IO ()     -> ListT IO ()
  lift ∷                             Maybe Int -> ListT Maybe Int

  Takes a normal monad and lifts it up into the the transformer.

  In GHCI:

  > a = (lift (Just 1) :: ListT Maybe Int)
  > :t a
  a :: ListT Maybe Int
-}

hello :: ListT IO ()
hello = do
  lift $ putStrLn "Hello"


{-
  Cannot run above function on the main application level since `main :: IO ()`. We need away to get to the `IO ()` that lives
  in the `ListT`.

  `runListT :: ListT m a -> m [a]`

  hello :: ListT IO ()

  runListT :: ListT IO () -> IO [()]

  This will get our monad out and run it
-}

-- run :: ListT IO () -> IO ()
-- run lio = runListT lio

main =
  runListT hello


test :: ListT IO ()
test = do
  x <- return [1, 2]
  y <- ListT $ return ['a', 'b']

  lift $ putStrLn (show (x, y))
