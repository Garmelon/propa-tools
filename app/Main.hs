module Main where

import Data.Void

import Props.Lambda.Term

yCombinator :: Term Void String String
yCombinator = Lambda "f"
  (App
    (Lambda "x" (App (Var 1) (App (Var 0) (Var 0))))
    (Lambda "x" (App (Var 1) (App (Var 0) (Var 0))))
  )

main :: IO ()
main = print yCombinator
