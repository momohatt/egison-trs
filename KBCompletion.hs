import Data.List
import Data.Maybe
import Prelude    hiding (Ordering)

data Term
  = Var String
  | Compound String [Term]
  deriving Eq

type Substitution = [(String, Term)]
type Ordering = (String, Int) -> (String, Int) -> Bool

instance Show Term where
  show (Var x) = x
  show (Compound f []) = f
  show (Compound "*" [Compound "*" [x, z], y]) = "(" ++ show x ++ " * " ++ show z ++ ") * " ++ show y
  show (Compound "*" [x, y]) = show x ++ " * " ++ show y
  show (Compound "+" [Compound "+" [x, z], y]) = "(" ++ show x ++ " + " ++ show z ++ ") + " ++ show y
  show (Compound "+" [x, y]) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Compound f xs) = f ++ "(" ++ intercalate ", " (map show xs) ++ ")"

data Equation = Eq (Term, Term)
  deriving Eq

instance Show Equation where
  show (Eq(l, r)) = show l ++ " = " ++ show r


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
  fv (Eq(lhs, rhs)) = nub $ fv lhs ++ fv rhs

  subst sigma (Eq(lhs, rhs)) = Eq (subst sigma lhs, subst sigma rhs)

  occur x (Eq(lhs, rhs)) = occur x lhs || occur x rhs


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


termMatch :: Substitution -> [(Term, Term)] -> Maybe Substitution
termMatch env [] = Just env
termMatch env ((Compound f xs, Compound g ys):oth)
  | f == g && length xs == length ys = termMatch env (zip xs ys ++ oth)
  | otherwise = Nothing
termMatch env ((Var x, t):oth) =
  case lookup x env of
    Nothing -> termMatch ((x, t) : env) oth
    Just t'
      | t == t' -> termMatch env oth
      | otherwise -> Nothing
termMatch env _ = Nothing


-- Non-variable subterms
subterms :: Term -> [(Term, Term -> Term)]
subterms (Var _) = []
subterms t@(Compound f xs) =
  (t, id) :
  (map (\(t, context) -> (t, \x -> Compound f $ context x)) $ mapSubterms [] xs)


mapSubterms :: [Term] -> [Term] -> [(Term, Term -> [Term])]
mapSubterms _ [] = []
mapSubterms prev (x:xs) =
  map (\(t, context) -> (t, \x -> prev ++ context x : xs)) (subterms x)
  ++ mapSubterms (prev ++ [x]) xs


crit1 :: Equation -> Equation -> [(Term, Term)]
crit1 (Eq(l1, r1)) (Eq(l2, r2)) =
  let subtms = subterms l1
   in foldl helper [] subtms
  where
    helper :: [(Term, Term)] -> (Term, Term -> Term) -> [(Term, Term)]
    helper acc (subtm, context) =
      case unify l2 subtm of
        Nothing -> acc
        Just sigma ->
          (subst sigma r1, subst sigma $ context r2) : acc


-- Implementation in the textbook
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

crit2 :: Equation -> Equation -> [(Term, Term)]
crit2 (Eq(l1, r1)) (Eq(l2, r2)) =
  overlaps (l1, r1) l2  $ \sigma t -> (subst sigma t, subst sigma r2)

criticalPairs' :: (Equation -> Equation -> [(Term, Term)])
               -> Equation -> Equation -> [(Term, Term)]
criticalPairs' crit tm1 tm2 =
  let (tm1', tm2') = renamePair (tm1, tm2)
   in if tm1 == tm2 then crit tm1' tm2'
                    else nub $ crit tm1' tm2' ++ crit tm2' tm1'

criticalPairs :: Equation -> Equation -> [(Term, Term)]
criticalPairs = criticalPairs' crit2


normalizeAndOrient :: (Term -> Term -> Bool) -> [Equation] -> (Term, Term)
                   -> Maybe Equation
normalizeAndOrient ord eqs (s, t) =
  let s' = rewrite eqs s
      t' = rewrite eqs t
      ret
        | ord s' t' = Just $ Eq (s', t')
        | ord t' s' = Just $ Eq (t', s')
        | otherwise = Nothing
      in ret


-- rewrite :: [Equation] -> Term -> Term
-- rewrite axioms tm =
--   case rewrite' axioms tm of
--     Just tm' -> rewrite axioms tm'
--     Nothing -> tm
--   where
--     rewrite' [] tm = Nothing
--     rewrite' ((Eq(s, t)):axioms) tm =
--       case rewrite'' (s, t) (subterms tm) of
--         Nothing -> rewrite' axioms tm
--         Just tm -> Just tm
--     rewrite'' (s, t) [] = Nothing
--     rewrite'' (s, t) ((subtm, ctx):xs) =
--       case termMatch [] [(s, subtm)] of
--         Just sigma -> Just $ ctx (subst sigma t)
--         Nothing -> rewrite'' (s, t) xs

rewrite :: [Equation] -> Term -> Term
rewrite axioms tm =
  case rewrite1 axioms tm of
    Just tm' -> rewrite axioms tm'
    Nothing ->
      case tm of
        Var _ -> tm
        Compound f xs ->
          let tm' = Compound f (map (rewrite axioms) xs)
           in if tm' == tm then tm else rewrite axioms tm'

rewrite1 :: [Equation] -> Term -> Maybe Term
rewrite1 [] t = Nothing
rewrite1 (Eq(l, r) : axioms) t =
  case termMatch [] [(l, t)] of
    Just sigma -> Just $ subst sigma r
    Nothing -> rewrite1 axioms t


reportStatus :: ([Equation], [(Term, Term)], [(Term, Term)]) -> IO ()
reportStatus (eqs, deferred, crits) = do
  putStrLn $ show (length eqs) ++ " equations and " ++
    show (length crits) ++ " pending critical pairs; " ++
      show (length deferred) ++ " deferred"


complete' :: (Term -> Term -> Bool)
         -> ([Equation], [(Term, Term)], [(Term, Term)])
         -> IO (Maybe [Equation])
complete' ord (eqs, [], []) =
  return $ Just eqs
complete' ord (eqs, deferred, []) =
  case find (isJust . (normalizeAndOrient ord eqs)) deferred of
    Just e -> complete' ord (eqs, filter (/= e) deferred, [e])
    Nothing -> do
      print eqs
      -- print deferred
      return Nothing
complete' ord (eqs, deferred, eq:oldcrits) =
  let triplets =
        (case normalizeAndOrient ord eqs eq of
           Nothing -> (eqs, eq:deferred, oldcrits)
           Just (Eq (s', t'))
             | s' == t'  -> (eqs, deferred, oldcrits)
             | otherwise ->
               let eq' = Eq(s', t')
                   eqs' = eq' : eqs
                   newcrits = foldr ((++) . (criticalPairs eq')) [] eqs'
                in (eqs', deferred, oldcrits ++ newcrits))
   in do
     reportStatus triplets
     complete' ord triplets


complete :: [String] -> [Equation] -> IO (Maybe [Equation])
complete ordList eqs =
  complete' ord (eqs, [], nub $ concat [criticalPairs e1 e2 | e1 <- eqs, e2 <- eqs])
    where
      ord = lpoGe $ weight ordList


-- lexicographic ordering between two sequences of alphabets
-- having the same length
lexOrd :: (Term -> Term -> Bool) -> [Term] -> [Term] -> Bool
lexOrd ord (h1:t1) (h2:t2)
  | ord h1 h2 = length t1 == length t2
  | otherwise = h1 == h2 && lexOrd ord t1 t2
lexOrd ord _ _ = False


lpoGt :: Ordering -> Term -> Term -> Bool
lpoGt w s t =
  case (s, t) of
    (_, Var x) -> s /= t &&  elem x (fv s)
    (Compound f xs, Compound g ys) ->
      any (\si -> lpoGe w si t) xs ||
        all (lpoGt w s) ys && (f == g && lexOrd (lpoGt w) xs ys ||
          w (f, length xs) (g, length ys))
    _ -> False


lpoGe :: Ordering -> Term -> Term -> Bool
lpoGe w s t = s == t || lpoGt w s t


weight :: [String] -> Ordering
weight list (f, n) (g, m)
  | f == g    = n > m
  | otherwise =
    let (Just fi) = elemIndex f list
        (Just gi) = elemIndex g list
     in fi < gi

--
-- Examples
--

x = Var "x"
y = Var "y"
z = Var "z"
a = Compound "a" []
b = Compound "b" []
c = Compound "c" []

zero = Compound "O" []
one = Compound "S" [zero]
two = Compound "S" [one]
three = Compound "S" [two]

axiomIkebuchi :: [Equation]
axiomIkebuchi =
  [Eq (mult (mult x y) (mult y z), y)]
    where mult x y = Compound "*" [x, y]

-- (x3 * x4 * x5) * x2 = x4 * x2
-- x4 * (x4 * x5) * x2 = x4 * x5,
-- (x0 * x3 * x4) * x4 = x3 * x4,
-- (x * y) * y * z = y]

axiomsOfGroup :: [Equation]
axiomsOfGroup =
  [Eq (mult e x, x),
   Eq (mult (i x) x, e),
   Eq (mult (mult x y) z, mult x (mult y z))]
     where
       mult x y = Compound "*" [x, y]
       e = Compound "e" []
       i x = Compound "i" [x]

axiomsOfNat :: [Equation]
axiomsOfNat =
  [Eq (add zero x, x),
   Eq (add (s x) y, add x (s y))]
     where
       s x = Compound "S" [x]
       add x y = Compound "+" [x, y]


-- let (x, y) = head $ criticalPairs (head axiomIkebuchi) (head axiomIkebuchi)
-- let Eq(z, _) = head axiomIkebuchi

-- unify (Compound "+" [(Compound "S" [x]), y]) (Compound "+" [one, two])

{-
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
