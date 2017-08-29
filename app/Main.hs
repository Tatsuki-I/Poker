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
main =  do (print . reverse . map length . group . sort . map (snd . checkHands) . comb 5) initCards
           -- Should be [4,32,624,3744,5108,10200,54912,123552,1098240,1302540]

-- main =  do mapM_ (print) $ map (\x -> (x, checkHands x)) $ comb 5 $ initCards
