{-# LANGUAGE PartialTypeSignatures #-}

import Data.List

data Term
  = Var String
  | Compound String [Term]
  deriving Eq

instance Show Term where
  show (Var x) = x
  show (Compound f []) = f
  show (Compound "*" [x, y]) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Compound f xs) = f ++ "(" ++ intercalate ", " (map show xs) ++ ")"

data Equation = Eq Term Term
  deriving Eq
type Substitution = [(String, Term)]

class Entity a where
  fv :: a -> [String]
  subst :: Substitution -> a -> a
  occur :: String -> a -> Bool

instance Entity Term where
  fv (Var x) = [x]
  fv (Compound f xs) = nub $ concatMap fv xs

  subst sigma (Var x) =
    case lookup x sigma of
      Just y -> y
      Nothing -> Var x
  subst sigma (Compound f xs) = Compound f $ map (subst sigma) xs

  occur x (Var y) = x == y
  occur x (Compound f xs) = any (occur x) xs

instance Entity Equation where
  fv (Eq lhs rhs) = nub $ fv lhs ++ fv rhs

  subst sigma (Eq lhs rhs) = Eq (subst sigma lhs) (subst sigma rhs)

  occur x (Eq lhs rhs) = occur x lhs || occur x rhs


renamePair :: Entity a => (a, a) -> (a, a)
renamePair (tm1, tm2) =
  let fvs1 = fv tm1
      fvs2 = fv tm2
      len1 = length fvs1
      len2 = length fvs2
      nms1 = map (\n -> Var $ "x" ++ show n) [0 .. (len1 - 1)]
      nms2 = map (\n -> Var $ "x" ++ show n) [len1 .. (len1 + len2 - 1)]
   in (subst (zip fvs1 nms1) tm1, subst (zip fvs2 nms2) tm2)


unify :: Term -> Term -> Maybe Substitution
unify (Var x) tm2@(Var _) = Just [(x, tm2)]
unify (Var x) tm2@(Compound _ _)
  | occur x tm2 = Nothing
  | otherwise   = Just [(x, tm2)]
unify tm1@(Compound _ _) (Var x)
  | occur x tm1 = Nothing
  | otherwise   = Just [(x, tm1)]
unify (Compound f []) (Compound g [])
  | f /= g    = Nothing
  | otherwise = Just []
unify (Compound f (x:xs)) (Compound g (y:ys))
  | f /= g    = Nothing
  | otherwise = do
    sigma <- unify x y
    let xs' = map (subst sigma) xs
    let ys' = map (subst sigma) ys
    res <- unify (Compound f xs') (Compound g ys')
    return $ sigma ++ res
unify _ _ = Nothing


listcases :: (Term -> (Substitution -> Term -> (Term, Term)) -> [(Term, Term)])
          -> (Substitution -> [Term] -> (Term, Term))
          -> [Term]
          -> [(Term, Term)]
          -> [(Term, Term)]
listcases fn rfn [] acc = acc
listcases fn rfn (x:xs) acc =
  fn x (\sigma x' -> rfn sigma (x':xs)) ++
    listcases fn (\sigma xs' -> rfn sigma (x:xs')) xs acc


overlaps :: (Term, Term) -> Term
         -> (Substitution -> Term -> (Term, Term))
         -> [(Term, Term)]
overlaps (l, r) (Var _) rfn = []
overlaps (l, r) tm@(Compound f xs) rfn =
  let acc = (case unify l tm of
               Just sth -> [rfn sth r]
               Nothing -> [])
   in listcases (overlaps (l, r)) (\sigma a -> rfn sigma (Compound f a)) xs acc


crit1 :: Equation -> Equation -> [(Term, Term)]
crit1 (Eq l1 r1) (Eq l2 r2) =
  overlaps (l1, r1) l2  $ \sigma t -> (subst sigma t, subst sigma r2)


criticalPairs :: Equation -> Equation -> [(Term, Term)]
criticalPairs tm1 tm2 =
  let (tm1', tm2') = renamePair (tm1, tm2)
   in if tm1 == tm2 then crit1 tm1' tm2'
                    else nub $ crit1 tm1' tm2' ++ crit1 tm2' tm1'


--
-- Examples
--

x = Var "x"
y = Var "y"
z = Var "z"

zero = Compound "O" []
one = Compound "S" [zero]
two = Compound "S" [one]
three = Compound "S" [two]

axiomIkebuchi :: [Equation]
axiomIkebuchi =
  [Eq (Compound "*" [(Compound "*" [x, y]), (Compound "*" [y, z])]) y]

-- unify (Compound "+" [(Compound "S" [x]), y]) (Compound "+" [one, two])

{-
(* ------------------------------------------------------------------------- *)
(* Orienting an equation.                                                    *)
(* ------------------------------------------------------------------------- *)

let normalize_and_orient ord eqs (Atom(R("=",[s;t]))) =
  let s' = rewrite eqs s and t' = rewrite eqs t in
  if ord s' t' then (s',t') else if ord t' s' then (t',s')
  else failwith "Can't orient equation";;

(* ------------------------------------------------------------------------- *)
(* Status report so the user doesn't get too bored.                          *)
(* ------------------------------------------------------------------------- *)

let status(eqs,def,crs) eqs0 =
  (* if eqs = eqs0 & (length crs) mod 1000 <> 0 then () else *)
  (print_string(string_of_int(length eqs)^" equations and "^
                string_of_int(length crs)^" pending critical pairs + "^
                string_of_int(length def)^" deferred");
   print_newline());;

(* ------------------------------------------------------------------------- *)
(* Completion main loop (deferring non-orientable equations).                *)
(* ------------------------------------------------------------------------- *)

let rec complete ord (eqs,def,crits) =
  match crits with
  | eq::ocrits ->
        let trip =
          try let (s',t') = normalize_and_orient ord eqs eq in
              if s' = t' then (eqs,def,ocrits) else
              let eq' = Atom(R("=",[s';t'])) in
              let eqs' = eq'::eqs in
              eqs',def,
              ocrits @ itlist ((@) ** critical_pairs eq') eqs' []
          with Failure _ -> (eqs,eq::def,ocrits) in
        status trip eqs; complete ord trip
  | _ -> if def = [] then eqs else
         let e = find (can (normalize_and_orient ord eqs)) def in
         complete ord (eqs,subtract def [e],[e]);;

(* ------------------------------------------------------------------------- *)
(* Interreduction.                                                           *)
(* ------------------------------------------------------------------------- *)

let rec interreduce dun eqs =
  match eqs with
    (Atom(R("=",[l;r])))::oeqs ->
        let dun' = if rewrite (dun @ oeqs) l <> l then dun
                   else mk_eq l (rewrite (dun @ eqs) r)::dun in
        interreduce dun' oeqs
  | [] -> rev dun;;

(* ------------------------------------------------------------------------- *)
(* Overall function with post-simplification (but not dynamically).          *)
(* ------------------------------------------------------------------------- *)

let complete_and_simplify wts eqs =
  let ord = lpo_ge (weight wts) in
  let eqs' = map (fun e -> let l,r = normalize_and_orient ord [] e in
                           mk_eq l r) eqs in
  (interreduce [] ** complete ord)
  (eqs',[],unions(allpairs critical_pairs eqs' eqs'));;
-}
