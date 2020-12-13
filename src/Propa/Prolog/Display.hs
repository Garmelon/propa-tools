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

displayStat :: T.Text -> [Term T.Text] -> T.Text
displayStat name [] = displayName name
displayStat name args
  = displayName name
  <> "("
  <> T.intercalate ", " (map displayTerm args)
  <> ")"

displayList :: Term T.Text -> T.Text
displayList (Stat "[|]" [a, b]) = "," <> displayTerm a <> displayList b
displayList (Stat "[]" [])      = "]"
displayList t                   = "|" <> displayTerm t <> "]"

displayTerm :: Term T.Text -> T.Text
displayTerm (Var v)             = v
displayTerm (Stat "[|]" [a, b]) = "[" <> displayTerm a <> displayList b
displayTerm (Stat name args)    = displayStat name args

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
displayResult
  = T.intercalate "\n"
  . map (\(k, v) -> k <> " = " <> displayTerm v)
  . filter (\(k, v) -> v /= Var k)
  . Map.assocs
