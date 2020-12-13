module Propa.Prolog.Types
  ( Stat(..)
  , Term(..)
  , Def(..)
  , Db
  ) where

import qualified Data.Text as T

data Stat a = Stat T.Text [Term a]
  deriving (Show, Eq)

instance Functor Stat where
  fmap f (Stat name terms) = Stat name $ fmap (fmap f) terms

instance Foldable Stat where
  foldMap f (Stat _ terms) = foldMap (foldMap f) terms

instance Traversable Stat where
  traverse f (Stat name terms) = Stat name <$> traverse (traverse f) terms

data Term a
  = TVar a
  | TStat (Stat a)
  deriving (Show, Eq)

instance Functor Term where
  fmap f (TVar a)  = TVar $ f a
  fmap f (TStat s) = TStat $ fmap f s

instance Foldable Term where
  foldMap f (TVar a)  = f a
  foldMap f (TStat s) = foldMap f s

instance Traversable Term where
  traverse f (TVar a)  = TVar <$> f a
  traverse f (TStat s) = TStat <$> traverse f s

data Def a = Def (Stat a) [Stat a]
  deriving (Show)

instance Functor Def where
  fmap f (Def stat stats) = Def (fmap f stat) (fmap f <$> stats)

instance Foldable Def where
  foldMap f (Def stat stats) = foldMap f stat <> foldMap (foldMap f) stats

instance Traversable Def where
  traverse f (Def stat stats) = Def <$> traverse f stat <*> traverse (traverse f) stats

type Db a = [Def a]
