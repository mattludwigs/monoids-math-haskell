module StateMo where

import Control.Monad.State

-- newtype State s a =
--   State { runState :: s -> (a, s) }

myState :: State String String
myState = state (\s -> ("one", "two"))


showResult :: Show a => State String a -> IO ()
showResult st =
  (putStrLn ("Frist: " ++ (show $ fst $ runState st ""))) >>
    (putStrLn ("Second: " ++ (show $ snd $ runState st "")))


showResult' :: Show a => State String a -> IO ()
showResult' st = do
  (first, second) <- return (runState st "any")
  putStrLn ("First: " ++ (show first))
  putStrLn ("Second: " ++ (show second))
