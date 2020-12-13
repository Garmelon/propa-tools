{-# LANGUAGE OverloadedStrings #-}

module Propa.Prolog.Unify
  ( run
  , runOne
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
  , cTerms  :: Map.Map Int (T.Text, [Term Int])
  } deriving (Show)

newContext :: [Def T.Text] -> Context
newContext db = Context db 0 Map.empty Map.empty

learnVar :: Int -> Int -> UniM ()
learnVar k v = modify $ \c -> c{cVars = Map.insert k v $ cVars c}

learnTerm :: Int -> T.Text -> [Term Int] -> UniM ()
learnTerm k name args = modify $ \c -> c{cTerms = Map.insert k (name, args) $ cTerms c}

-- | Look up a variable, first repeatedly in the var map and then the term map.
-- Returns statements unchanged.
--
-- If this returns a variable, then that variable is not bound.
lookupVar :: Term Int -> UniM (Term Int)
lookupVar (Var v) = do
  c <- get
  let lastV = follow (cVars c) v
  pure $ case cTerms c Map.!? lastV of
    Nothing           -> Var lastV
    Just (name, args) -> Stat name args
lookupVar t@(Stat _ _) = pure t

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

satisfy :: Term Int -> UniM ()
satisfy (Var _) = pure ()
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
    (Var v) -> pure $ Var v
    (Stat name args) -> do
      args2 <- traverse resolveVars args
      pure $ Stat name args2

newtype Terms a = Terms { unTerms :: [Term a] }

instance Functor Terms where
  fmap f (Terms ts) = Terms $ fmap (fmap f) ts

instance Foldable Terms where
  foldMap f (Terms ts) = foldMap (foldMap f) ts

run :: Db T.Text -> [Term T.Text] -> [Map.Map T.Text (Term T.Text)]
run db terms = map fst $ runStateT helper $ newContext db
  where
    helper = do
      (terms2, vmap) <- understand $ Terms terms
      satisfyTerms $ unTerms terms2
      tmap <- traverse (resolveVars . Var) vmap
      c <- get
      let naming = findVarNaming vmap (cVars c) $ Map.elems tmap
      pure $ fmap (naming Map.!) <$> tmap

runOne :: Db T.Text -> Term T.Text -> [Map.Map T.Text (Term T.Text)]
runOne db term = run db [term]
