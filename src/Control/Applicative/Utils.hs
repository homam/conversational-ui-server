module Control.Applicative.Utils (
  (<.>)
) where

(<.>) :: Functor f => (a -> b) -> (c -> f a) -> c -> f b
f1 <.> f2 = fmap f1 . f2
infixr 9 <.>
