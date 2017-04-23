module ListMo where

doLoop :: [Int] -> [Int]
doLoop lis = do
  x <- lis
  return (x + 1)


pairs l1 l2 = do
  x <- l1
  do
    y <- l2
    return (x, y)

{-
  Apply Rule 2

  that is

  y <- l2 == l2 >>= (\y -> ___ stuff ___)
-}
pairs' l1 l2 = do
  x <- l1
  do
    l2 >>= (\y -> return (x, y))


{-
  Apply rule 3
-}
pairs'' l1 l2 = do
  x <- l1
  l2 >>= (\y -> return (x, y))


pairs''' l1 l2 = do
  x <- l1
  y <- l2
  return (x, y)


getTwoInputs = do
  putStrLn "Input? "
  x <- getLine
  putStrLn "Input? "
  y <- getLine
  putStrLn $ show (pairs x y)
