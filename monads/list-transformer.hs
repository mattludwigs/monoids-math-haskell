module ListTransformer where

import Control.Monad.List

{-
  Below is the first pass in trying to understand
  Monad Transformers
-}

-- newtype ListT m a =
--   ListT { runListT :: m [a] }

type Lio = ListT IO

-- wrong :: Lio ()
-- wrong =
--   putStrLn "hello"

hello :: ListT IO ()
hello = do
  lift $ putStrLn "Hello World"

runHello = runListT hello

{-
  The `return` is putting the list into the `ListT IO`
-}
inListT :: ListT IO [Int]
inListT = do
  return [1,3]

{-
  The `return` is putting the list into the `List`
-}
listMe :: [[Int]]
listMe =
  return [1,2]

test :: Lio ()
test = do
  x <- return [1, 2]
  y <- ListT $ return ['a', 'b']
  lift $ putStrLn (show (x, y))

-- bad = do
--   x <- [1, 2]
--   y <- ["a", "b"]
--   putStrLn (show (x, y))
