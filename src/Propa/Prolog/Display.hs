{-# LANGUAGE OverloadedStrings #-}

module Propa.Prolog.Display
  ( displayTerm
  , displayTerms
  , displayDef
  , displayDefs
  , displayResult
  ) where

import           Data.Char
import           Data.List

import qualified Data.Map           as Map
import qualified Data.Text          as T

import           Propa.Prolog.Types

displayName :: T.Text -> T.Text
displayName name
  | T.all isLower name = name
  | otherwise          = "\"" <> escaped <> "\""
  where
    escaped = foldl' (\s (a, b) -> T.replace a b s) name
      [ ("\\", "\\\\")
      , ("\n", "\\n")
      , ("\r", "\\r")
      , ("\t", "\\t")
      ]

displayStat :: Stat T.Text -> T.Text
displayStat (Stat "[|]" [a, b]) = "[" <> displayTerm a <> displayList b
displayStat (Stat name []) = displayName name
displayStat (Stat name args)
  = displayName name
  <> "("
  <> T.intercalate ", " (map displayTerm args)
  <> ")"

displayList :: Term T.Text -> T.Text
displayList (TStat (Stat "[|]" [a, b])) = "," <> displayTerm a <> displayList b
displayList (TStat (Stat "[]" []))      = "]"
displayList t                           = "|" <> displayTerm t <> "]"

displayTerm :: Term T.Text -> T.Text
displayTerm (TVar v)  = v
displayTerm (TStat s) = displayStat s

displayTerms :: [Term T.Text] -> T.Text
displayTerms terms = T.intercalate ",\n" (map displayTerm terms) <> "."

displayDef :: Def T.Text -> T.Text
displayDef (Def stat []) = displayStat stat <> "."
displayDef (Def stat stats)
  =  displayStat stat
  <> " :-\n"
  <> T.intercalate ",\n" (map (\t -> "    " <> displayStat t) stats)
  <> "."

displayDefs :: [Def T.Text] -> T.Text
displayDefs = T.intercalate "\n" . map displayDef

displayResult :: Map.Map T.Text (Term T.Text) -> T.Text
displayResult
  = T.intercalate "\n"
  . map (\(k, v) -> k <> " = " <> displayTerm v)
  . filter (\(k, v) -> v /= TVar k)
  . Map.assocs
