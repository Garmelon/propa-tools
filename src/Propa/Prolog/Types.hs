module Propa.Prolog.Types
  ( Term(..)
  , Def(..)
  , Db
  ) where

import qualified Data.Text as T

data Term a
  = Var a
  | Stat T.Text [Term a]
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

data Def a = Def T.Text [Term a] [Term a]
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

type Db a = [Def a]
