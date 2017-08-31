module Main where

import Lib
import Poker
import TexasHoldEm
import System.Random.Shuffle
import Data.List

main :: IO ()
main =  do sls <- shuffleM initCards
           print $ initTexas sls
           print $ map checkHand $ comb 5 $ _hole (initTexas sls) ++ _community (initTexas sls)

           -- Should be [4,32,624,3744,5108,10200,54912,123552,1098240,1302540]

-- main =  do mapM_ (print) $ map (\x -> (x, checkHands x)) $ comb 5 $ initCards
