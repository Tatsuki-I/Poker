module Main where

import Poker
import System.Random.Shuffle
import Data.List

{-
main :: IO ()
main =  do sls <- shuffleM initCards
          print $ sort $ take 5 sls
          print $ checkHands $ take 5 sls
-}
main :: IO ()
main =  do print $ map length $ group $ sort $ map checkHands $ comb 5 initCards

-- main =  do mapM_ (print) $ map (\x -> (x, checkHands x)) $ comb 5 $ initCards
