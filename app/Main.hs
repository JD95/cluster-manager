module Main where

import qualified Cluster (app)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.app
