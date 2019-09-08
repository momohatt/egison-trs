module Types where

import           Data.List
import           Data.Maybe

data Term
  = Var String
  | Compound String [Term]
  deriving Eq

type Substitution = [(String, Term)]
type Ordering = (String, Int) -> (String, Int) -> Bool
type CriticalPair = ((Term, Term), (Equation, Equation))

instance Show Term where
  show (Var x) = x
  show (Compound f []) = f
  show (Compound "*" [Compound "*" [x, z], y]) = "(" ++ show x ++ " * " ++ show z ++ ") * " ++ show y
  show (Compound "*" [x, y]) = show x ++ " * " ++ show y
  show (Compound "+" [Compound "+" [x, z], y]) = "(" ++ show x ++ " + " ++ show z ++ ") + " ++ show y
  show (Compound "+" [x, y]) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Compound f xs) = f ++ "(" ++ intercalate ", " (map show xs) ++ ")"

newtype Equation = Eq (Term, Term)
  deriving Eq

instance Show Equation where
  show (Eq(l, r)) = show l ++ " = " ++ show r


class Entity a where
  fv :: a -> [String]
  subst1 :: (String, Term) -> a -> a
  subst :: Substitution -> a -> a
  occur :: String -> a -> Bool

instance Entity Term where
  fv (Var x) = [x]
  fv (Compound f xs) = nub $ concatMap fv xs

  subst1 (x, t) (Var y)
    | x == y    = t
    | otherwise = Var y
  subst1 (x, t) (Compound f xs) = Compound f $ map (subst1 (x, t)) xs

  subst sigma (Var x) = fromMaybe (Var x) (lookup x sigma)
  subst sigma (Compound f xs) = Compound f $ map (subst sigma) xs

  occur x (Var y) = x == y
  occur x (Compound f xs) = any (occur x) xs

instance Entity Equation where
  fv (Eq(lhs, rhs)) = nub $ fv lhs ++ fv rhs

  subst1 xt (Eq(lhs, rhs))   = Eq (subst1 xt lhs, subst1 xt rhs)
  subst sigma (Eq(lhs, rhs)) = Eq (subst sigma lhs, subst sigma rhs)

  occur x (Eq(lhs, rhs)) = occur x lhs || occur x rhs
