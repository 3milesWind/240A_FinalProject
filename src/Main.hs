module Main where
--  (  main
--  ) where

import Level (pickLevel)
import UI (main2)
main :: IO ()
main = do 
   pickLevel>>=(\x->UI.main2 x)

