module Props.Lambda.Display
  ( displayTerm
  ) where

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
findConstNames = undefined

makeVarNamesUnique :: Term e c (Maybe Name) -> Term e c (Maybe Name)
makeVarNamesUnique = undefined

findVarNames :: Term e c (Maybe Name) -> Term e c Name
findVarNames = undefined

displayTerm :: (e -> T.Text) -> Term e (Maybe Name) (Maybe Name) -> T.Text
displayTerm = undefined
