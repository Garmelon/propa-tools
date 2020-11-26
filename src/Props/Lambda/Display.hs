{-# LANGUAGE OverloadedStrings #-}

module Props.Lambda.Display
  ( findConstNames
  , makeVarNamesUnique
  , findVarNames
  , displayTerm
  , displayTerm'
  ) where

import           Data.Maybe
import           Numeric.Natural

import           Control.Monad.Trans.State
import qualified Data.Set                  as Set
import qualified Data.Text                 as T

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

chooseUnique :: (Ord a) => Set.Set a -> [a] -> a
chooseUnique taken = head . dropWhile (`Set.member` taken)

makeNameUnique :: Set.Set Name -> Name -> Name
makeNameUnique taken name
  = chooseUnique taken
  $ zipWith (<>) (repeat name)
  $ "" : map (T.pack . show) [(2::Integer) ..]

findConstNames :: Term e (Maybe Name) v -> Term e Name v
findConstNames term = evalState (helper term) (Set.fromList $ catMaybes $ consts term)
  where
    helper (Var i) = pure $ Var i
    helper (Const c) = do
      taken <- get
      let name = fromMaybe (chooseUnique taken constNames) c
      put $ Set.insert name taken
      pure $ Const name
    helper (Lambda v t) = Lambda v <$> helper t
    helper (App l r) = App <$> helper l <*> helper r
    helper (Ext e) = pure $ Ext e

makeVarNamesUnique :: Term e Name (Maybe Name) -> Term e Name (Maybe Name)
makeVarNamesUnique term = helper (Set.fromList $ consts term) term
  where
    helper _     (Var i) = Var i
    helper _     (Const c) = Const c
    helper taken (Lambda v t) = case v of
      Nothing -> Lambda Nothing $ helper taken t
      Just name ->
        let newName = makeNameUnique taken name
        in  Lambda (Just newName) $ helper (Set.insert name taken) t
    helper taken (App l r) = App (helper taken l) (helper taken r)
    helper _     (Ext e) = Ext e

findVarNames :: Term e Name (Maybe Name) -> Term e Name Name
findVarNames term = helper (Set.fromList $ consts term) term
  where
    helper _     (Var i) = Var i
    helper _     (Const c) = Const c
    helper taken (Lambda v t) =
      let name = fromMaybe (chooseUnique taken varNames) v
      in  Lambda name $ helper (Set.insert name taken) t
    helper taken (App l r) = App (helper taken l) (helper taken r)
    helper _     (Ext e) = Ext e

displayTerm :: (e -> T.Text) -> Term e Name Name -> T.Text
displayTerm f = dTerm f []

displayTerm' :: (e -> T.Text) -> Term e (Maybe Name) (Maybe Name) -> T.Text
displayTerm' f = displayTerm f . findVarNames . makeVarNamesUnique . findConstNames

nth :: [a] -> Natural -> Maybe a
nth []     _ = Nothing
nth (x:_)  0 = Just x
nth (_:xs) n = nth xs $ n - 1

varName :: [Name] -> Natural -> Name
varName vs i = fromMaybe e $ nth vs i
  where
    e = "[" <> T.pack (show i) <> "]"

dTerm :: (e -> T.Text) -> [Name] -> Term e Name Name -> T.Text
dTerm _ vs (Var i) = varName vs i
dTerm _ _  (Const c) = c
dTerm f vs (Lambda v t) = "λ" <> v <> dLambda (v:vs) t
  where
    dLambda ws (Lambda w u) = " " <> w <> dLambda (w:ws) u
    dLambda ws u            = ". " <> dTerm f ws u
dTerm f vs (App l r) = dLeft l <> " " <> dRight r
  where
    dLeft t@(Lambda _ _) = "(" <> dTerm f vs t <> ")"
    dLeft t              = dTerm f vs t
    dRight t@(Lambda _ _) = "(" <> dTerm f vs t <> ")"
    dRight t@(App _ _)    = "(" <> dTerm f vs t <> ")"
    dRight t              = dTerm f vs t
dTerm f _  (Ext e) = f e
