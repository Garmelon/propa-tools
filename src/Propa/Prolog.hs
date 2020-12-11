module Propa.Prolog where

import           Control.Monad
import           Data.Foldable

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Data.Map                  as Map
import qualified Data.Set                  as Set

data Term a
  = Var a
  | Stat String [Term a]
  deriving (Show)

instance Functor Term where
  fmap f (Var a)          = Var $ f a
  fmap f (Stat name args) = Stat name $ fmap (fmap f) args

instance Foldable Term where
  foldMap f (Var a)       = f a
  foldMap f (Stat _ args) = foldMap (foldMap f) args

instance Traversable Term where
  traverse f (Var a)          = Var <$> f a
  traverse f (Stat name args) = Stat name <$> traverse (traverse f) args

data Def a = Def String [Term a] [Term a]
  deriving (Show)

instance Functor Def where
  fmap f (Def dName dArgs dTerms) = Def dName (fmap f <$> dArgs) (fmap f <$> dTerms)

instance Foldable Def where
  foldMap f (Def _ dArgs dTerms) = foldMap (foldMap f) dArgs <> foldMap (foldMap f) dTerms

instance Traversable Def where
  traverse f (Def dName dArgs dTerms)
    =   Def dName
    <$> traverse (traverse f) dArgs
    <*> traverse (traverse f) dTerms

data Context = Context
  { cDb     :: [Def String]
  , cVarIdx :: Int
  , cVars   :: Map.Map Int Int
  , cTerms  :: Map.Map Int (String, [Term Int])
  } deriving (Show)

newContext :: [Def String] -> Context
newContext db = Context db 0 Map.empty Map.empty

learnVar :: Int -> Int -> UniM ()
learnVar k v = modify $ \c -> c{cVars = Map.insert k v $ cVars c}

learnTerm :: Int -> String -> [Term Int] -> UniM ()
learnTerm k name args = modify $ \c -> c{cTerms = Map.insert k (name, args) $ cTerms c}

-- | Look up a variable, first in the var map and then the term map. Returns
-- statements unchanged.
--
-- If this returns a variable, then that variable is unbound.
lookupVar :: Term Int -> UniM (Term Int)
lookupVar t@(Stat _ _) = pure t
lookupVar t@(Var v) = do
  c <- get
  case cVars c Map.!? v of
    Just v' -> lookupVar (Var v')
    Nothing -> pure $ case cTerms c Map.!? v of
      Nothing           -> t
      Just (name, args) -> Stat name args

-- | A simple state monad transformer over the list monad for easy backtracking.
-- Needs to be changed when implementing cuts.
type UniM = StateT Context []

-- | A faster version of 'nub'.
fastNub :: (Ord a) => [a] -> [a]
fastNub = Set.toList . Set.fromList

varMap :: (Foldable a) => a String -> UniM (Map.Map String Int)
varMap a = do
  c <- get
  let i = cVarIdx c
      vars = fastNub $ toList a
      vmap = Map.fromList $ zip vars [i..]
  put c{cVarIdx = i + Map.size vmap}
  pure vmap

-- | Convert a definition's variables to unique integers that are not already in
-- use in the current context.
understand :: (Functor a, Foldable a) => a String -> UniM (a Int, Map.Map String Int)
understand a = do
  vmap <- varMap a
  pure (fmap (vmap Map.!) a, vmap)

satisfy :: Term Int -> UniM ()
satisfy (Var _)          = undefined
satisfy (Stat name args) = do
  c <- get
  (Def dName dArgs dTerms, _) <- understand =<< lift (cDb c)
  lift $ guard $ name == dName -- Not sure if 'lift' is really necessary
  unifyTerms args dArgs
  satisfyTerms dTerms

satisfyTerms :: [Term Int] -> UniM ()
satisfyTerms = traverse_ satisfy

unify :: Term Int -> Term Int -> UniM ()
unify t1 t2 = do
  t1' <- lookupVar t1
  t2' <- lookupVar t2
  case (t1', t2') of
    (Stat name1 args1, Stat name2 args2) -> do
      lift $ guard $ name1 == name2
      unifyTerms args1 args2
    (Var v1, Stat name2 args2) -> learnTerm v1 name2 args2
    (Stat name1 args1, Var v2) -> learnTerm v2 name1 args1
    (Var v1, Var v2) -> learnVar v1 v2 -- The order shouldn't really matter

unifyTerms :: [Term Int] -> [Term Int] -> UniM ()
unifyTerms t1 t2 = do
  lift $ guard $ length t1 == length t2
  sequenceA_ $ zipWith unify t1 t2

run :: Term String -> UniM (Map.Map String (Term Int))
run t = do
  (t2, vmap) <- understand t
  satisfy t2
  traverse (lookupVar . Var) vmap

exampleDb :: [Def String]
exampleDb =
  [ Def "food" [Stat "burger" []] []
  , Def "food" [Stat "sandwich" []] []
  , Def "meal" [Var "X"] [Stat "food" [Var "X"]]
  ]

burgerIsMeal :: Term String
burgerIsMeal = Stat "meal" [Stat "burger" []]

whatIsMeal :: Term String
whatIsMeal = Stat "meal" [Var "X"]
