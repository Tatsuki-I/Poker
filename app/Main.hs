module Main where

import Poker
import System.Random.Shuffle
import Data.List

main :: IO ()
main = do sls <- shuffleM initCards
          print $ sort $ take 5 sls
          print $ checkHands $ take 5 sls

