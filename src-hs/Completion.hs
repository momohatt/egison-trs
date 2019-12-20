import           Data.List
import           Data.Maybe
import           Prelude    hiding (Ordering, succ)

import           Types

renamePair :: Substitutable a => (a, a) -> (a, a)
renamePair (tm1, tm2) =
  let fvs1 = fv tm1
      fvs2 = fv tm2
      sigma1 = zip fvs1 $ map (\n -> Var $ "x" ++ show n) [0 .. ]
      sigma2 = zip fvs2 $ map (\n -> Var $ "x" ++ show n) [(length fvs1) .. ]
   in (subst sigma1 tm1, subst sigma2 tm2)

compose :: Substitution -> (String, Term) -> Substitution
compose sigma (x, t) =
  sigma ++ [(x, subst sigma t)]

unify :: Substitution -> [(Term, Term)] -> Maybe Substitution
unify _ [] = Just []
unify sigma ((s, t):tms) =
  case (s, t) of
    (Var x, Compound _ _) | occur x t -> Nothing
    (Var x, _) -> do
      let tms' = map (\(t1, t2) -> (subst1 (x, t) t1, subst1 (x, t) t2)) tms
      sigma' <- unify sigma tms'
      return $ sigma' `compose` (x, t)
    (Compound _ _, Var y) | occur y s -> Nothing
    (_, Var y) -> do
      let tms' = map (\(t1, t2) -> (subst1 (y, s) t1, subst1 (y, s) t2)) tms
      sigma' <- unify sigma tms'
      return $ sigma' `compose` (y, s)
    (Compound f xs, Compound g ys)
      | f /= g                 -> Nothing
      | length xs /= length ys -> Nothing
      | otherwise              -> unify sigma (zip xs ys ++ tms)

termMatch :: Substitution -> [(Term, Term)] -> Maybe Substitution
termMatch env eqs =
  case eqs of
    [] -> Just env
    (Compound f xs, Compound g ys):oth
      | f == g && length xs == length ys -> termMatch env (zip xs ys ++ oth)
    (Var x, t):oth -> case lookup x env of
                        Nothing -> termMatch ((x, t) : env) oth
                        Just t' | t == t' -> termMatch env oth
                        _ -> Nothing
    _ -> Nothing

-- Non-variable subterms
subterms :: Term -> [(Term, Term -> Term)]
subterms (Var _) = []
subterms t@(Compound f xs) =
  (t, id) :
  map (\(t, context) -> (t, Compound f . context)) (mapSubterms [] xs)

mapSubterms :: [Term] -> [Term] -> [(Term, Term -> [Term])]
mapSubterms _ [] = []
mapSubterms prev (x:xs) =
  map (\(t, context) -> (t, \x -> prev ++ context x : xs)) (subterms x)
  ++ mapSubterms (prev ++ [x]) xs

crit1 :: Equation -> Equation -> [CriticalPair]
crit1 e1@(Eq(l1, r1)) e2@(Eq(l2, r2)) =
  let subtms = subterms l1
   in foldl helper [] subtms
  where
    helper :: [CriticalPair] -> (Term, Term -> Term) -> [CriticalPair]
    helper acc (subtm, context) =
      case unify [] [(l2, subtm)] of
        Nothing -> acc
        Just sigma ->
          (subst sigma r1, subst sigma $ context r2) : acc

criticalPairs :: Equation -> Equation -> [CriticalPair]
criticalPairs tm1 tm2 =
  let (tm1', tm2') = renamePair (tm1, tm2)
   in if tm1 == tm2 then crit1 tm1' tm2'
                    else crit1 tm1' tm2' ++ crit1 tm2' tm1'

orient :: (Term -> Term -> Bool) -> (Term, Term) -> Maybe Equation
orient ord (s, t)
  | ord s t   = Just $ Eq (s, t)
  | ord t s   = Just $ Eq (t, s)
  | otherwise = Nothing

normalizeAndOrient :: (Term -> Term -> Bool) -> [Equation] -> (Term, Term)
                   -> Maybe Equation
normalizeAndOrient ord eqs (s, t) =
  orient ord (rewrite eqs s, rewrite eqs t)

rewrite :: [Equation] -> Term -> Term
rewrite axioms tm =
  case rewrite' axioms tm of
    Just tm' -> rewrite axioms tm'
    Nothing -> tm
  where
    rewrite' [] tm = Nothing
    rewrite' ((Eq(s, t)):axioms) tm =
      case rewrite'' (s, t) (subterms tm) of
        Nothing -> rewrite' axioms tm
        Just tm -> Just tm
    rewrite'' (s, t) [] = Nothing
    rewrite'' (s, t) ((subtm, ctx):xs) =
      case termMatch [] [(s, subtm)] of
        Just sigma -> Just $ ctx (subst sigma t)
        Nothing -> rewrite'' (s, t) xs

reportStatus :: ([Equation], [CriticalPair], [CriticalPair]) -> [Equation] -> IO ()
reportStatus (eqs, crits, deferred) eqs0 = do
  if eqs == eqs0 then return () else do
    putStrLn $ show (length eqs) ++ " equations and " ++
      show (length crits) ++ " pending critical pairs; " ++
        show (length deferred) ++ " deferred"


complete' :: (Term -> Term -> Bool)
         -> ([Equation], [CriticalPair], [CriticalPair])
         -> IO (Maybe [Equation])
complete' ord (eqs, [], []) =
  return $ Just eqs
complete' ord (eqs, [], deferred) =
  case find (isJust . normalizeAndOrient ord eqs) deferred of
    Just e -> complete' ord (eqs, [e], filter (/= e) deferred)
    Nothing -> do
      print eqs
      return Nothing
complete' ord (eqs, (s, t):oldcrits, deferred) = do
  let s' = rewrite eqs s
  let t' = rewrite eqs t
  let triplets
        | s' == t' = (eqs, oldcrits, deferred)
        | otherwise =
          case orient ord (s', t') of
            Nothing -> (eqs, oldcrits, (s', t'):deferred)
            Just (Eq (s', t'))
              | s' == t'  -> (eqs, deferred, oldcrits)
              | otherwise ->
                let eq' = Eq(s', t')
                    eqs' = interreduce (eq' : eqs)
                    newcrits = concatMap (criticalPairs eq') eqs'
                 in (eqs', oldcrits ++ newcrits, deferred)
  reportStatus triplets eqs
  complete' ord triplets

complete :: [String] -> [Equation] -> IO (Maybe [Equation])
complete ordList eqs =
  complete' ord (eqs, nub $ concat [criticalPairs e1 e2 | e1 <- eqs, e2 <- eqs], [])
    where
      ord = lpoGe $ weight ordList

interreduce' :: [Equation] -> [Equation] -> [Equation]
interreduce' dun eqs =
  case eqs of
    [] -> reverse dun
    Eq(l, r):oeqs ->
      let dun' = if rewrite (dun ++ oeqs) l /= l
                    then dun
                    else Eq(l, rewrite (dun ++ eqs) r) : dun
       in interreduce' dun' oeqs

interreduce :: [Equation] -> [Equation]
interreduce = interreduce' []

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
    (_, Var x) -> s /= t && elem x (fv s)
    (Compound f xs, Compound g ys) ->
      any (\si -> lpoGe w si t) xs ||
        all (lpoGt w s) ys &&
          (f == g && lexOrd (lpoGt w) xs ys ||
            w (f, length xs) (g, length ys))
    _ -> False

lpoGe :: Ordering -> Term -> Term -> Bool
lpoGe w s t = s == t || lpoGt w s t

weight :: [String] -> Ordering
weight list (f, n) (g, m)
  | f == g    = n > m
  | otherwise =
    let fi = fromJust $ elemIndex f list
        gi = fromJust $ elemIndex g list
     in fi > gi

--
-- Examples
--

x = Var "x"
y = Var "y"
z = Var "z"
a = Compound "a" []
b = Compound "b" []
c = Compound "c" []

zero  = Compound "O" []
one   = Compound "S" [zero]
two   = Compound "S" [one]
three = Compound "S" [two]

mult x y = Compound "*" [x, y]
plus x y = Compound "+" [x, y]
succ x   = Compound "S" [x]

axiomCentralGroupoid :: [Equation]
axiomCentralGroupoid =
  [Eq (mult (mult x y) (mult y z), y)]

e   = Compound "e" []
i x = Compound "i" [x]

axiomsOfGroup :: [Equation]
axiomsOfGroup =
  [Eq (mult e x, x),
   Eq (mult (i x) x, e),
   Eq (mult (mult x y) z, mult x (mult y z))
  ]

axiomsOfGroupComplete :: [Equation]
axiomsOfGroupComplete =
  axiomsOfGroup ++
    [Eq (i (mult y x), mult (i x) (i y)),    -- (y * x)' = x' * y'
     Eq (mult (i y) (mult y x), x),          -- y' * (y * x) = x
     Eq (mult x (i x), e),                   -- x * x' = e
     Eq (i e, e),                            -- e' = e
     Eq (i (i x), x),                        -- x'' = x
     Eq (mult x e, x),                       -- x * e = x
     Eq (mult x (mult (i x) y), y)           -- x * (x' * y) = y
    ]

axiomsOfNat :: [Equation]
axiomsOfNat =
  [Eq (plus zero x, x),
   Eq (plus (succ x) y, plus x (succ y))]
