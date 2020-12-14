{-# LANGUAGE OverloadedStrings #-}

module Propa.Prolog.Debug
  ( parseAndRun
  ) where

import qualified Data.Text            as T
import qualified Data.Text.IO         as T

import           Propa.Prolog.Display
import           Propa.Prolog.Parse
import           Propa.Prolog.Unify

parseAndRun :: String -> String -> IO ()
parseAndRun dbStr statsStr = T.putStrLn $ case results of
  Left e   -> e
  Right [] -> "No."
  Right rs -> T.intercalate "\n" rs
  where
    results = do
      db <- parseDb "<db>" $ T.pack dbStr
      stats <- parseStats "<query>" $ T.pack statsStr
      pure $ map displayResult $ run db stats
