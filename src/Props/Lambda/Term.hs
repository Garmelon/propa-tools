module Props.Lambda.Term
  ( Term(..)
  , vars
  , consts
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
  -- ^ Term extension (set @e@ to 'Void' if you don't need this)
  deriving (Show)

vars :: Term e c v -> [v]
vars (Lambda v t) = v : vars t
vars (App l r)    = vars l <> vars r
vars _            = []

consts :: Term e c v -> [c]
consts (Const c)    = [c]
consts (Lambda _ t) = consts t
consts (App l r)    = consts l <> consts r
consts _            = []
