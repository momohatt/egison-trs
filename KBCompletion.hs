import           Data.List
import           Data.Maybe
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language (haskellDef)
import qualified Text.ParserCombinators.Parsec.Token    as P
import           Prelude                                hiding (Ordering, succ)

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

renamePair :: Entity a => (a, a) -> (a, a)
renamePair (tm1, tm2) =
  let fvs1 = fv tm1
      fvs2 = fv tm2
      len1 = length fvs1
      len2 = length fvs2
      nms1 = map (\n -> Var $ "x" ++ show n) [0 .. (len1 - 1)]
      nms2 = map (\n -> Var $ "x" ++ show n) [len1 .. (len1 + len2 - 1)]
   in (subst (zip fvs1 nms1) tm1, subst (zip fvs2 nms2) tm2)

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
          ((subst sigma r1, subst sigma $ context r2), (e1, e2)) : acc

-- -- Implementation in the textbook
-- listcases :: (Term -> (Substitution -> Term -> (Term, Term)) -> [(Term, Term)])
--           -> (Substitution -> [Term] -> (Term, Term))
--           -> [Term]
--           -> [(Term, Term)]
--           -> [(Term, Term)]
-- listcases fn rfn [] acc = acc
-- listcases fn rfn (x:xs) acc =
--   fn x (\sigma x' -> rfn sigma (x':xs)) ++
--     listcases fn (\sigma xs' -> rfn sigma (x:xs')) xs acc
--
-- overlaps :: (Term, Term) -> Term
--          -> (Substitution -> Term -> (Term, Term))
--          -> [(Term, Term)]
-- overlaps (l, r) (Var _) rfn = []
-- overlaps (l, r) tm@(Compound f xs) rfn =
--   let acc = (case unify [] [(l, tm)] of
--                Just sth -> [rfn sth r]
--                Nothing -> [])
--    in listcases (overlaps (l, r)) (\sigma a -> rfn sigma (Compound f a)) xs acc
--
-- crit2 :: Equation -> Equation -> [CriticalPair]
-- crit2 (Eq(l1, r1)) (Eq(l2, r2)) =
--   overlaps (l1, r1) l2  $ \sigma t -> (subst sigma t, subst sigma r2)

criticalPairs' :: (Equation -> Equation -> [CriticalPair])
               -> Equation -> Equation -> [CriticalPair]
criticalPairs' crit tm1 tm2 =
  let (tm1', tm2') = renamePair (tm1, tm2)
   in if tm1 == tm2 then crit tm1' tm2'
                    else nub $ crit tm1' tm2' ++ crit tm2' tm1'

criticalPairs :: Equation -> Equation -> [CriticalPair]
criticalPairs = criticalPairs' crit1

orient :: (Term -> Term -> Bool) -> (Term, Term) -> Maybe Equation
orient ord (s, t)
  | ord s t   = Just $ Eq (s, t)
  | ord t s   = Just $ Eq (t, s)
  | otherwise = Nothing

normalizeAndOrient :: (Term -> Term -> Bool) -> [Equation] -> (Term, Term)
                   -> Maybe Equation
normalizeAndOrient ord eqs (s, t) =
  let s' = rewrite eqs s
      t' = rewrite eqs t
   in orient ord (s', t')

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

reportStatus :: ([Equation], [CriticalPair], [CriticalPair]) -> [Equation] -> IO ()
reportStatus (eqs, deferred, crits) eqs0 = do
  -- if eqs == eqs0 then return () else do
    print $ head crits
    putStrLn $ show (length eqs) ++ " equations and " ++
      show (length crits) ++ " pending critical pairs; " ++
        show (length deferred) ++ " deferred"
    -- print $ head eqs

complete' :: (Term -> Term -> Bool)
         -> ([Equation], [CriticalPair], [CriticalPair])
         -> IO (Maybe [Equation])
complete' ord (eqs, [], []) =
  return $ Just eqs
complete' ord (eqs, deferred, []) =
  case find (isJust . normalizeAndOrient ord eqs . fst) deferred of
    Just e -> complete' ord (eqs, filter (/= e) deferred, [e])
    Nothing -> do
      print eqs
      -- print deferred
      return Nothing
complete' ord (eqs, deferred, ((s, t), e):oldcrits) = do
  let s' = rewrite eqs s
  let t' = rewrite eqs t
  let triplets
        | s' == t' = (eqs, deferred, oldcrits)
        | otherwise =
          case orient ord (s', t') of
            Nothing -> (eqs, ((s', t'), e):deferred, oldcrits)
            Just (Eq (s', t'))
              | s' == t'  -> (eqs, deferred, oldcrits)
              | otherwise ->
                let eq' = Eq(s', t')
                    eqs' = eq' : eqs
                    newcrits = concatMap (criticalPairs eq') eqs'
                 in (eqs', deferred, oldcrits ++ newcrits)
  reportStatus triplets eqs
  complete' ord triplets

complete :: [String] -> [Equation] -> IO (Maybe [Equation])
complete ordList eqs =
  complete' ord (eqs, [], nub $ concat [criticalPairs e1 e2 | e1 <- eqs, e2 <- eqs])
    where
      ord = lpoGe $ weight ordList

interreduce :: [Equation] -> [Equation] -> [Equation]
interreduce dun eqs =
  case eqs of
    [] -> reverse dun
    Eq(l, r):oeqs ->
      let dun' = if rewrite (dun ++ oeqs) l /= l
                    then dun
                    else Eq(l, rewrite (dun ++ eqs) r) : dun
       in interreduce dun' oeqs


completeAndSimplify :: [String] -> [Equation] -> IO (Maybe [Equation])
completeAndSimplify wts eqs = do
  let triple = (eqs, [], concat [criticalPairs eq1 eq2 | eq1 <- eqs, eq2 <- eqs])
  axioms <- complete' ord triple
  case axioms of
    Nothing -> return Nothing
    Just axioms' -> return . Just $ interreduce [] axioms'
    where
      ord = lpoGe (weight wts)


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
    let (Just fi) = elemIndex f list
        (Just gi) = elemIndex g list
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

zero = Compound "O" []
one = Compound "S" [zero]
two = Compound "S" [one]
three = Compound "S" [two]

mult x y = Compound "*" [x, y]
plus x y = Compound "+" [x, y]
succ x = Compound "S" [x]

axiomIkebuchi :: [Equation]
axiomIkebuchi =
  [Eq (mult (mult x y) (mult y z), y)]

-- eq3 : (x3 * x4 * x5) * x2 = x4 * x2  <- ((x3 * x4 * x5) * x2,(x0 * x4 * x2) * x2)
-- eq2 : x4 * (x4 * x5) * x2 = x4 * x5,
-- eq1 : (x0 * x3 * x4) * x4 = x3 * x4,
-- eq0 : (x * y) * y * z = y
--
-- >>> criticalPairs eq0 eq1
-- [(x1,(x0 * x1) * x1 * x4),
--  ((x0 * x4) * x5,(x0 * x4) * (x0 * x4 * x5) * x2),
--  ((x0 * x1) * x1 * x2,(x0 * x1) * x1 * x5),
--  (x3 * x4,x3 * x4)]
-- >>> criticalPairs eq0 eq2
-- [(x3 * x4 * x5,(x0 * x3 * x4 * x5) * x4 * x2),
--  (x1,(x4 * x1) * x1 * x2),
--  ((x0 * x1) * x1 * x2,(x3 * x1) * x1 * x2),
--  (x4 * x5,x4 * x5)]
-- >>> criticalPairs eq1 eq2
-- [((x3 * x4 * x5) * x1,(x3 * x4 * x5) * (x4 * x1) * x2),
--  (x0 * x4 * x5,x0 * x4 * x2),
--  (x4 * (x4 * x1) * x2,(x3 * x4 * x1) * (x4 * x1) * x2),
--  ((x3 * x1) * x5,(x3 * x1) * x5)]
-- >>> criticalPairs eq2 eq2
-- [((x3 * x4 * x5) * x2,(x0 * x4 * x2) * x2),
--  (x1 * x2,(x4 * x1 * x2) * x2),
--  (x1 * x2,x1 * x2)]

--[<<(x5 * x0 * x4) * x4 = (x2 * x0 * x4) * x4>>;
-- <<(x0 * x3 * x4) * x4 = x3 * x4>>;
-- <<x3 * x4 = x3 * x4>>]

eq0 = Eq (mult (mult x y) (mult y z), y)
eq1 = Eq (mult (Var "x4") (mult (mult (Var "x4") (Var "x5")) (Var "x2")), mult (Var "x4") (Var "x5"))
eq2 = Eq (mult (mult (Var "x0") (mult (Var "x3") (Var "x4"))) (Var "x4"), mult (Var "x3") (Var "x4"))

e = Compound "e" []
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
    [Eq (i (mult y x), mult (i x) (i y)),  -- (y * x)' = x' * y'
     Eq (mult (i y) (mult y x), x),          -- y' * (y * x) = x
     Eq (mult x (i x), e),                   -- x * x' = e
     Eq (i e, e),                            -- e' = e
     Eq (i (i x), x),                        -- x'' = x
     Eq (mult x e, x),                       -- x * e = x
     Eq (mult x (mult (i x) y), y)           -- x * (x' * y) = y
    ]

-- z * y * i(x) * i(z * i(x * i(y))) = e
-- mult z (mult y (mult (i x) (i (mult z (i (mult x (i y)))))))


axiomsOfNat :: [Equation]
axiomsOfNat =
  [Eq (plus zero x, x),
   Eq (plus (succ x) y, plus x (succ y))]



-- let (x, y) = head $ criticalPairs (head axiomIkebuchi) (head axiomIkebuchi)
-- let Eq(z, _) = head axiomIkebuchi

-- unify [] (plus (succ x) y, plus one two)

----------------------------------- parser -------------------------------------

lexer :: P.TokenParser ()
lexer = P.makeTokenParser haskellDef

parens = P.parens lexer

inSpaces :: Parser a -> Parser ()
inSpaces p = spaces >> p >> spaces

equation :: Parser Equation
equation = do
  l <- parseTerm
  inSpaces $ char '='
  r <- parseTerm
  return $ Eq (l, r)

parseTerm :: Parser Term
parseTerm = try compound
   <|> var

var :: Parser Term
var = do
  char 'x'
  d <- many1 digit
  return . Var $ 'x' : d

compound :: Parser Term
compound = try parseMult <|> try parsePlus <|> app

parseMult :: Parser Term
parseMult = do
  l <- atom
  inSpaces $ char '*'
  r <- try parseMult <|> atom
  return $ Compound "*" [l, r]

parsePlus :: Parser Term
parsePlus = do
  l <- atom
  inSpaces $ char '+'
  r <- try parsePlus <|> atom
  return $ Compound "+" [l, r]

app :: Parser Term
app = do
  f <- many1 letter
  char '('
  xs <- many1 parseTerm
  char ')'
  return $ Compound f xs

atom :: Parser Term
atom = try var
    <|> try app
    <|> parens parseTerm
