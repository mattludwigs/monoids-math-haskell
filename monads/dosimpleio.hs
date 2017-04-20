module Dosimpleio where
{-
  Practice code from "From Simple IO to Monad Transformers" by J Adrian Zimmer.
-}

main = do
  line1 <- getLine
  putStrLn line1

  line2 <- getLine
  putStrLn line2


monadDo = do
  getLine >>=
    (\x ->
      (getLine >>= putStrLn) >> putStrLn x
    )


eqMainDo = do
  x <- getLine
  do x <- getLine
     putStrLn x
  putStrLn x

eqMainDo' =
  getLine >>=
    (\x ->
      -- mon >>= (\x -> f x)
      (getLine >>= putStrLn) >> putStrLn x
    )

eqMainDo'' = do
 x <- getLine
 -- mon >>= (\x -> f x)
 do getLine >>= putStrLn -- Apply rule 3 here
 putStrLn x


-- eqMainDo''' = do
--   x <- getLine
--   getLine >>= putStrLn -- after 3rd rule applied
--   putStrLn


quiz =
  (getLine >>= putStrLn) >> (getLine >>= putStrLn)




{-
  Add Do Block (Rule 3)
-}
quizAnswerStep1 = do
  (getLine >>= putStrLn) >> (getLine >>= putStrLn)

{-
  Break apart seq (Rule 2)
-}
quizAnswerStep2 = do
  (getLine >>= putStrLn)
  (getLine >>= putStrLn)


{-
  Convert into variable binding (Rule 2)
-}
quizAnswerStep3 = do
  x <- getLine
  putStrLn x

  y <- getLine
  putStrLn y
