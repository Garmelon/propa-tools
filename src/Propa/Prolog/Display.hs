{-# LANGUAGE OverloadedStrings #-}

module Propa.Prolog.Display
  ( displayTerm
  , displayTerms
  , displayDef
  , displayDefs
  , displayResult
  ) where

import qualified Data.Map           as Map
import qualified Data.Text          as T

import           Propa.Prolog.Types

displayStat :: T.Text -> [Term T.Text] -> T.Text
displayStat name []   = name
displayStat name args = name <> "(" <> T.intercalate ", " (map displayTerm args) <> ")"

displayTerm :: Term T.Text -> T.Text
displayTerm (Var v)          = v
displayTerm (Stat name args) = displayStat name args

displayTerms :: [Term T.Text] -> T.Text
displayTerms terms = T.intercalate ",\n" (map displayTerm terms) <> "."

displayDef :: Def T.Text -> T.Text
displayDef (Def name args []) = displayStat name args <> "."
displayDef (Def name args terms)
  =  displayStat name args
  <> " :-\n"
  <> T.intercalate ",\n" (map (\t -> "    " <> displayTerm t) terms)
  <> "."

displayDefs :: [Def T.Text] -> T.Text
displayDefs = T.intercalate "\n" . map displayDef

displayResult :: Map.Map T.Text (Term T.Text) -> T.Text
displayResult = T.intercalate "\n" . map (\(k, v) -> k <> " = " <> displayTerm v) . Map.assocs
