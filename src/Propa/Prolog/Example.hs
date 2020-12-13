{-# LANGUAGE OverloadedStrings #-}

module Propa.Prolog.Example where

import qualified Data.Text          as T

import           Propa.Prolog.Types

db :: Db T.Text
db =
  [ Def "append" [Stat "nil" [], Var "Y", Var "Y"] []
  , Def "append" [Stat "cons" [Var "X", Var "XS"], Var "Y", Stat "cons" [Var "X", Var "Z"]] [Stat "append" [Var "XS", Var "Y", Var "Z"]]
  ]

l12 :: Term T.Text
l12 = Stat "cons" [Stat "1" [], Stat "cons" [Stat "2" [], Stat "nil" []]]

l345 :: Term T.Text
l345 = Stat "cons" [Stat "3" [], Stat "cons" [Stat "4" [], Stat "cons" [Stat "5" [], Stat "nil" []]]]
