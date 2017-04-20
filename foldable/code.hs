module Code where

{-

  class Foldable (t :: * -> *) where
    fold :: (Monoid m, Foldable t) => t m -> m
    foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

    foldMap must map each element of the stuture `t` to the monoid

-}
