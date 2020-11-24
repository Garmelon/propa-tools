{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           Props
import           Yesod

main :: IO ()
main = warp 3000 App
