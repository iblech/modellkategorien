{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
module Main where

newtype Ran k t a = MkRan { runRan :: forall r. (a -> k r) -> t r }
-- folgt einfach aus der Ende-Formel für Ran und der Berechnung
-- von Ende(S) als Menge der S-Keile mit Spitze 1.
-- Siehe auch http://comonad.com/reader/2008/kan-extension-iii/.

instance Functor (Ran k t) where
    fmap f phi = MkRan $ \u -> runRan phi (u . f)

universal :: (Functor s) => (forall r. s (k r) -> t r) -> s a -> Ran k t a
universal alpha v = MkRan $ \u -> alpha (fmap u v)

data Coend s = forall a. Coend (s a a)
{-
  Eigentlich sollte das lauten:

      Coend (exists a. s a a / (lmap f x = rmap f x für alle f : a -> b, x : s a b))

  Wegen Parametrizität respektieren aber Funktionen Coend f -> r (so wie
  definiert) automatisch lmap/rmap.
-}

newtype End s = End { runEnd :: forall a. s a a }
-- Hier ist es ähnlich. Die Werte vom Typ End s erfüllen automatisch die
-- Keilbedingung.

-- Ran k t c = End ((a,b) |-> (c -> k a) -> t b)

-- Lan k t c = Coend ((a,b) |-> (k a -> c) x t b)
