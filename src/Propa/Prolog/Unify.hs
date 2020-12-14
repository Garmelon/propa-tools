{-# LANGUAGE OverloadedStrings #-}

module Propa.Prolog.Unify
  ( run
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import           Data.List
import           Data.Tuple

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Data.Map                  as Map
import qualified Data.Set                  as Set
import qualified Data.Text                 as T

import           Propa.Prolog.Types

-- General utility functions

-- | Start at a value and follow the map's entries until the end of the chain of
-- references.
follow :: (Ord a) => (b -> Maybe a) -> Map.Map a b -> b -> b
follow f m b = maybe b (follow f m) $ (m Map.!?) =<< f b

-- | Deduplicates the elements of a finite list. Doesn't preserve the order of
-- the elements. Doesn't work on infinite lists.
deduplicate :: (Ord a) => [a] -> [a]
deduplicate = Set.toList . Set.fromList

-- Now the fun begins...

data Context = Context
  { cDb     :: Db T.Text
  , cVarIdx :: Int
  , cTerms  :: Map.Map Int (Term Int)
  } deriving (Show)

newContext :: [Def T.Text] -> Context
newContext db = Context db 0 Map.empty

bindTerm :: Int -> Term Int -> UniM ()
bindTerm k v = modify $ \c -> c{cTerms = Map.insert k v $ cTerms c}

-- | Look up a variable, first repeatedly in the var map and then the term map.
-- Returns statements unchanged.
--
-- If this returns a variable, then that variable is not bound.
lookupTerm :: Term Int -> UniM (Term Int)
lookupTerm t = do
  c <- get
  pure $ follow tVar (cTerms c) t

-- | A simple state monad transformer over the list monad for easy backtracking.
-- Needs to be changed when implementing cuts.
type UniM = StateT Context []

varMap :: (Foldable a) => a T.Text -> UniM (Map.Map T.Text Int)
varMap a = do
  c <- get
  let i = cVarIdx c
      vars = deduplicate $ toList a
      vmap = Map.fromList $ zip vars [i..]
  put c{cVarIdx = i + Map.size vmap}
  pure vmap

-- | Convert a definition's variables to unique integers that are not already in
-- use in the current context.
understand :: (Functor a, Foldable a) => a T.Text -> UniM (a Int, Map.Map T.Text Int)
understand a = do
  vmap <- varMap a
  pure (fmap (vmap Map.!) a, vmap)

satisfyStat :: Stat Int -> UniM ()
satisfyStat stat = do
  c <- get
  (Def dStat dStats, _) <- understand =<< lift (cDb c)
  unifyStat stat dStat
  satisfyStats dStats

satisfyStats :: [Stat Int] -> UniM ()
satisfyStats = traverse_ satisfyStat

unifyStat :: Stat Int -> Stat Int -> UniM ()
unifyStat (Stat name1 args1) (Stat name2 args2) = do
  guard $ name1 == name2
  unifyTerms args1 args2

unifyTerm :: Term Int -> Term Int -> UniM ()
unifyTerm t1 t2 = do
  t1' <- lookupTerm t1
  t2' <- lookupTerm t2
  case (t1', t2') of
    (TStat s1, TStat s2) -> unifyStat s1 s2
    (TInt i1, TInt i2)   -> guard $ i1 == i2
    (TVar v, t)          -> bindTerm v t
    (t, TVar v)          -> bindTerm v t
    (_, _)               -> empty

unifyTerms :: [Term Int] -> [Term Int] -> UniM ()
unifyTerms t1 t2 = do
  guard $ length t1 == length t2
  sequenceA_ $ zipWith unifyTerm t1 t2

-- Figuring out how to display the result of the unification

-- | An infinite list of possible variable names: @A@, @B@, ..., @A1@, @B1@,
-- ..., @A2@, ...
varNames :: [T.Text]
varNames = do
  num <- "" : map (T.pack . show) [(1::Integer)..]
  char <- alphabet
  pure $ char <> num
  where
    alphabet = map T.singleton ['A'..'Z']

-- | Find a naming (Map from integer to name) for all variables in a list of
-- terms based on the original variable names and the variable mapping. Attempts
-- to map variables to known variables instead of a common unknown variable.
findVarNaming :: Map.Map T.Text Int -> Map.Map Int (Term Int) -> [Term Int] -> Map.Map Int T.Text
findVarNaming known vars terms =
  let knownLookedUp :: Map.Map T.Text Int
      knownLookedUp = Map.mapMaybe (tVar . follow tVar vars . TVar) known
      knownNaming = Map.fromList $ reverse $ map swap $ Map.toList knownLookedUp
      knownNames = Map.keysSet known
      knownVars = Map.keysSet knownNaming
      termVars = Set.fromList $ concatMap toList terms
      unknownVars = termVars Set.\\ knownVars
      availVarNames = filter (not . (`Set.member` knownNames)) varNames
      unknownNaming = Map.fromList $ zip (sort $ Set.toList unknownVars) availVarNames
  in  knownNaming <> unknownNaming

-- | Recursively set variables to their most resolved term.
resolveVars :: Term Int -> UniM (Term Int)
resolveVars t = do
  t2 <- lookupTerm t
  case t2 of
    (TVar v)                 -> pure $ TVar v
    (TInt i)                 -> pure $ TInt i
    (TStat (Stat name args)) -> TStat . Stat name <$> traverse resolveVars args

-- | Helper type so I can resolve variables in multiple statements
-- simultaneously.
newtype Stats a = Stats { unStats :: [Stat a] }

instance Functor Stats where
  fmap f (Stats ts) = Stats $ fmap (fmap f) ts

instance Foldable Stats where
  foldMap f (Stats ts) = foldMap (foldMap f) ts

run :: Db T.Text -> [Stat T.Text] -> [Map.Map T.Text (Term T.Text)]
run db stats = map fst $ runStateT helper $ newContext db
  where
    helper = do
      (stats2, vmap) <- understand $ Stats stats
      satisfyStats $ unStats stats2
      tmap <- traverse (resolveVars . TVar) vmap
      c <- get
      let naming = findVarNaming vmap (cTerms c) $ Map.elems tmap
      pure $ fmap (naming Map.!) <$> tmap
