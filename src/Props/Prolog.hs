module Props.Prolog where

import           Control.Monad

import qualified Data.Map      as Map

data Term
  = Variable String
  | Statement String [Term]
  deriving (Show)

type VarMap = Map.Map String Term

lookupTerm :: VarMap -> Term -> Term
lookupTerm _   t@(Statement _ _) = t
lookupTerm env t@(Variable s) = case env Map.!? s of
  Nothing -> t
  Just t' -> lookupTerm env t'

unifyStatements :: String -> [Term] -> String -> [Term] -> VarMap -> [VarMap]
unifyStatements a as b bs env = do
  guard $ a == b
  guard $ length as == length bs
  foldr (>=>) pure (zipWith unify as bs) env

unify :: Term -> Term -> VarMap -> [VarMap]
unify t1 t2 env = case (lookupTerm env t1, lookupTerm env t2) of
  (Statement a as, Statement b bs) -> unifyStatements a as b bs env
  (Variable a, b) -> [Map.insert a b env]
  (a, Variable b) -> [Map.insert b a env]

unify' :: Term -> Term -> [VarMap]
unify' t1 t2 = unify t1 t2 Map.empty
