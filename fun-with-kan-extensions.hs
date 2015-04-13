{-# LANGUAGE RankNTypes #-}
module Main where

newtype Ran k t a = MkRan { runRan :: forall r. (a -> k r) -> t r }

instance Functor (Ran k t) where
    fmap f phi = MkRan $ \u -> runRan phi (u . f)

universal :: (Functor s) => (forall r. s (k r) -> t r) -> s a -> Ran k t a
universal alpha v = MkRan $ \u -> alpha (fmap u v)

-- Weiter ausbauen!
