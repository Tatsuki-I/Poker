module Main where

import Poker
import TexasHoldEm
import Control.Lens
import System.Random.Shuffle
-- import Data.List

main :: IO ()
main =  do sls <- shuffleM initCards
           print $ initTexas ps sls
           print $ getHighestHand $ (head (initTexas ps sls ^. players) ^. hole) ++ _community (initTexas ps sls)
           print $ getHighestHand $ (last (initTexas ps sls ^. players) ^. hole) ++ _community (initTexas ps sls)
           putStrLn $ case compare 
                             (fst $ getHighestHand $ (head (initTexas ps sls ^. players) ^. hole) ++ _community (initTexas ps sls))
                             (fst $ getHighestHand $ (last (initTexas ps sls ^. players) ^. hole) ++ _community (initTexas ps sls))
                             of
                        EQ -> "Check!!"
                        LT -> (last ps ^. name) ++ " Win!!"
                        GT -> (head ps ^. name) ++ " Win!!"
            where ps = [initPlayer "Player1", initPlayer "Player2"]
