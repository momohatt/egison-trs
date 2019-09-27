{-# LANGUAGE QuasiQuotes #-}

module Utils where

import           Conrtol.Egison

data UnorderedPair a b = Pair a b
instance (Matcher a, Matcher b) => Matcher (UnorderedPair a b)

class PairPat m a where
  pair :: (Matcher m, a ~ (b1, b2), m ~ (Pair m1 m2)) => Pattern b1 m1 ctx xs -> Pattern b2 m2 (ctx :++: xs) ys -> Pattern a m ctx (xs :++: ys)

instance (Matcher m1, Matcher m2) => PairPat (UnorderedPair m1 m2) (a1, a2) where
  pair p1 p2 = Pattern
    (\_ (Pair m1 m2) (t1, t2) ->
      [MCons (MAtom p1 m1 t1) (MCons (MAtom p2 m2 t2) MNil),
       MCons (MAtom p2 m2 t2) (MCons (MAtom p1 m1 t1) MNil)])
