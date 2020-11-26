{-# LANGUAGE OverloadedStrings #-}

module Props.Lambda
  ( Term(..)
  , displayTerm
  ) where

import qualified Data.Set  as Set
import qualified Data.Text as T

type Name = T.Text
type PreferredName = Maybe Name

-- | Lambda calculus term using De Bruijn indexing
data Term a
  = Var PreferredName Int
  | Lambda PreferredName (Term a)
  | App (Term a) (Term a)
  | Native a
  deriving (Show)

displayTerm :: (Set.Set Name -> a -> T.Text) -> Term a -> T.Text
displayTerm = undefined
