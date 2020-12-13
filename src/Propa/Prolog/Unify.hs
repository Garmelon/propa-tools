{-# LANGUAGE OverloadedStrings #-}

module Propa.Prolog.Unify
  ( run
  ) where

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
follow :: (Ord a) => Map.Map a a -> a -> a
follow m v = maybe v (follow m) $ m Map.!? v

-- | Deduplicates the elements of a finite list. Doesn't preserve the order of
-- the elements. Doesn't work on infinite lists.
deduplicate :: (Ord a) => [a] -> [a]
deduplicate = Set.toList . Set.fromList

-- Now the fun begins...

data Context = Context
  { cDb     :: Db T.Text
  , cVarIdx :: Int
  , cVars   :: Map.Map Int Int
  , cStats  :: Map.Map Int (Stat Int)
  } deriving (Show)

newContext :: [Def T.Text] -> Context
newContext db = Context db 0 Map.empty Map.empty

bindVar :: Int -> Int -> UniM ()
bindVar k v = modify $ \c -> c{cVars = Map.insert k v $ cVars c}

bindStat :: Int -> Stat Int -> UniM ()
bindStat k s = modify $ \c -> c{cStats = Map.insert k s $ cStats c}

-- | Look up a variable, first repeatedly in the var map and then the term map.
-- Returns statements unchanged.
--
-- If this returns a variable, then that variable is not bound.
lookupVar :: Term Int -> UniM (Term Int)
lookupVar (TVar v) = do
  c <- get
  let lastV = follow (cVars c) v
  pure $ case cStats c Map.!? lastV of
    Nothing -> TVar lastV
    Just s  -> TStat s
lookupVar t@(TStat _) = pure t

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

satisfy :: Stat Int -> UniM ()
satisfy s = do
  c <- get
  (Def dStat dStats, _) <- understand =<< lift (cDb c)
  unifyStat s dStat
  satisfyStats dStats

satisfyStats :: [Stat Int] -> UniM ()
satisfyStats = traverse_ satisfy

unifyStat :: Stat Int -> Stat Int -> UniM ()
unifyStat (Stat name1 args1) (Stat name2 args2) = do
  guard $ name1 == name2
  unifyTerms args1 args2

unify :: Term Int -> Term Int -> UniM ()
unify t1 t2 = do
  t1' <- lookupVar t1
  t2' <- lookupVar t2
  case (t1', t2') of
    (TStat s1, TStat s2) -> unifyStat s1 s2
    (TVar v, TStat s)    -> bindStat v s
    (TStat s, TVar v)    -> bindStat v s
    (TVar v1, TVar v2)   -> bindVar v1 v2 -- The order shouldn't really matter

unifyTerms :: [Term Int] -> [Term Int] -> UniM ()
unifyTerms t1 t2 = do
  guard $ length t1 == length t2
  sequenceA_ $ zipWith unify t1 t2

-- Figuring out how to display the result of the unification

varNames :: [T.Text]
varNames = do
  num <- "" : map (T.pack . show) [(1::Integer)..]
  char <- alphabet
  pure $ char <> num
  where
    alphabet = map T.singleton ['A'..'Z']

findVarNaming :: Map.Map T.Text Int -> Map.Map Int Int -> [Term Int] -> Map.Map Int T.Text
findVarNaming known vars terms =
  let knownLookedUp = fmap (follow vars) known
      knownNaming = Map.fromList $ reverse $ map swap $ Map.toList knownLookedUp
      knownNames = Map.keysSet known
      knownVars = Map.keysSet knownNaming
      termVars = Set.fromList $ concatMap toList terms
      unknownVars = termVars Set.\\ knownVars
      availVarNames = filter (not . (`Set.member` knownNames)) varNames
      unknownNaming = Map.fromList $ zip (sort $ Set.toList unknownVars) availVarNames
  in  knownNaming <> unknownNaming

resolveVars :: Term Int -> UniM (Term Int)
resolveVars t = do
  t2 <- lookupVar t
  case t2 of
    (TVar v) -> pure $ TVar v
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
      let naming = findVarNaming vmap (cVars c) $ Map.elems tmap
      pure $ fmap (naming Map.!) <$> tmap
