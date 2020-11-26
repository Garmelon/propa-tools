{-# LANGUAGE OverloadedStrings #-}

module Props.Lambda.Display
  ( findConstNames
  , makeVarNamesUnique
  , findVarNames
  , displayTerm
  ) where

import Data.Maybe
import           Numeric.Natural

import qualified Data.Text         as T

import           Props.Lambda.Term

type Name = T.Text

varNames :: [Name]
varNames = chars ++ (mappend <$> constNames <*> chars)
  where
    chars = map T.singleton ['a'..'z']

constNames :: [Name]
constNames = chars ++ (mappend <$> constNames <*> chars)
  where
    chars = map T.singleton ['A'..'Z']

findConstNames :: Term e (Maybe Name) v -> Term e Name v
findConstNames = mapConsts (fromMaybe "[]") -- TODO implement

makeVarNamesUnique :: Term e c (Maybe Name) -> Term e c (Maybe Name)
makeVarNamesUnique = id -- TODO implement

findVarNames :: Term e c (Maybe Name) -> Term e c Name
findVarNames = mapVars (fromMaybe "[]") -- TODO implement

displayTerm :: (e -> T.Text) -> Term e Name Name -> T.Text
displayTerm f = dTerm f []

nth :: [a] -> Natural -> Maybe a
nth [] _ = Nothing
nth (x:_) 0 = Just x
nth (_:xs) n = nth xs $ n - 1

varName :: [Name] -> Natural -> Name
varName vs i = fromMaybe e $ nth vs i
  where
    e = "[" <> T.pack (show i) <> "]"

dTerm :: (e -> T.Text) -> [Name] -> Term e Name Name -> T.Text
dTerm _ vs (Var i) = varName vs i
dTerm _ _  (Const c) = c
dTerm f vs (Lambda v t) = "λ" <> v <> ". " <> dTerm f (v:vs) t
dTerm f _  (Ext e) = f e
dTerm f vs (App l r) = dLeft l <> " " <> dRight r
  where
    dLeft t@(Lambda _ _) = "(" <> dTerm f vs t <> ")"
    dLeft t = dTerm f vs t
    dRight t@(Lambda _ _) = "(" <> dTerm f vs t <> ")"
    dRight t@(App _ _) = "(" <> dTerm f vs t <> ")"
    dRight t = dTerm f vs t
