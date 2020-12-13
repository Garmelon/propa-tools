{-# LANGUAGE OverloadedStrings #-}

module Propa.Prolog.Debug
  ( parseAndRun
  ) where

import qualified Data.Text            as T
import qualified Data.Text.IO         as T

import           Propa.Prolog.Display
import           Propa.Prolog.Parse
import           Propa.Prolog.Unify

parseAndRun :: T.Text -> T.Text -> IO ()
parseAndRun dbText statsText = T.putStrLn $ case results of
  Left e   -> e
  Right [] -> "No."
  Right rs -> T.intercalate "\n" rs
  where
    results = do
      db <- parseDb "<input>" dbText
      stats <- parseStats "<input>" statsText
      pure $ map displayResult $ run db stats
