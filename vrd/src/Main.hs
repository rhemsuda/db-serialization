{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.Dom

main :: IO ()
main = mainWidget $ do
  el "h1" $ text "VRD"
  el "p" $ text "Deposit vape pods here"
