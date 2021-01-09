{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.Dom

main :: IO ()
main = mainWidget $ do
  el "div" $ text "Welcome to Reflex"
  el "p" $ text "This is going to be a beautiful application"
