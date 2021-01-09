module Main where

import Network.Wai
import Network.Wai.Handler.Warp

import Api (app)
import Database (migrateDb)

main :: IO ()
main = do
  migrateDb
  run 8081 app
