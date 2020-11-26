-- | This module contains 'Term', the base type for lambda expressions. It also
-- contains a few utility functions for operating on it.

module Props.Lambda.Term
  ( Term(..)
  , vars
  , mapVars
  , consts
  , mapConsts
  ) where

import           Numeric.Natural

-- | Lambda calculus term using De Bruijn indexing and expanded to deal with
-- naming complexity and extensions.
data Term e c v
  = Var Natural
  -- ^ Variable using a De Bruijn index
  | Const c
  -- ^ Constant living outside the variable namespace
  | Lambda v (Term e c v)
  -- ^ Lambda definition
  | App (Term e c v) (Term e c v)
  -- ^ Lambda application
  | Ext e
  -- ^ Term extension (set @e@ to 'Data.Void' if you don't need this)
  deriving (Show)

-- | All of a term's variable names in order from left to right.
vars :: Term e c v -> [v]
vars (Lambda v t) = v : vars t
vars (App l r)    = vars l <> vars r
vars _            = []

-- | Map over the variable names.
mapVars :: (a -> b) -> Term e c a -> Term e c b
mapVars _ (Var i)      = Var i
mapVars _ (Const c)    = Const c
mapVars f (Lambda a t) = Lambda (f a) (mapVars f t)
mapVars f (App l r)    = App (mapVars f l) (mapVars f r)
mapVars _ (Ext e)      = Ext e

-- | All of a term's constant names in order from left to right.
consts :: Term e c v -> [c]
consts (Const c)    = [c]
consts (Lambda _ t) = consts t
consts (App l r)    = consts l <> consts r
consts _            = []

-- | Map over the constant names.
mapConsts :: (a -> b) -> Term e a v -> Term e b v
mapConsts _ (Var i)      = Var i
mapConsts f (Const c)    = Const (f c)
mapConsts f (Lambda v t) = Lambda v (mapConsts f t)
mapConsts f (App l r)    = App (mapConsts f l) (mapConsts f r)
mapConsts _ (Ext e)      = Ext e
