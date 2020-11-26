{-# LANGUAGE OverloadedStrings #-}

module Props.Lambda.Term
  ( Term(..)
  , vars
  , mapVars
  , consts
  , mapConsts
  , termI
  , termY
  ) where

import           Numeric.Natural

import qualified Data.Text       as T

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

mapVars :: (a -> b) -> Term e c a -> Term e c b
mapVars _ (Var i)      = Var i
mapVars _ (Const c)    = Const c
mapVars f (Lambda a t) = Lambda (f a) (mapVars f t)
mapVars f (App l r)    = App (mapVars f l) (mapVars f r)
mapVars _ (Ext e)      = Ext e

consts :: Term e c v -> [c]
consts (Const c)    = [c]
consts (Lambda _ t) = consts t
consts (App l r)    = consts l <> consts r
consts _            = []

mapConsts :: (a -> b) -> Term e a v -> Term e b v
mapConsts _ (Var i)      = Var i
mapConsts f (Const c)    = Const (f c)
mapConsts f (Lambda v t) = Lambda v (mapConsts f t)
mapConsts f (App l r)    = App (mapConsts f l) (mapConsts f r)
mapConsts _ (Ext e)      = Ext e

termI :: Term e T.Text T.Text
termI = Lambda "x" (Var 0)

termY :: Term e T.Text T.Text
termY = Lambda "f" $ App
  (Lambda "x" $ App (Var 1) $ App (Var 0) (Var 0))
  (Lambda "x" $ App (Var 1) $ App (Var 0) (Var 0))
