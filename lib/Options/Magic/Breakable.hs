module Options.Magic.Breakable where

import           RIO

data Breakable a = Break a | Continue a

instance Functor Breakable where
  fmap f (Continue a) = Continue (f a)
  fmap f (Break a)    = Break (f a)

onlyOnce :: (a -> a) -> Breakable a -> Breakable a
onlyOnce f (Continue a) = Break (f a)
onlyOnce _ brk          = brk

peel :: Breakable a -> a
peel (Break a)    = a
peel (Continue a) = a
