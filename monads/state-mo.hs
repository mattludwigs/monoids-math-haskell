module StateMo where

{-
  s -> (a, s)

  where s is the state and a is the internal object

  newtype State s a =
    State { runState :: s -> (a, s) }

  State monad is a maker of monads 

-}
