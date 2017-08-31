module Main where

import Lib
import Poker
import TexasHoldEm
import System.Random.Shuffle
-- import Data.List

main :: IO ()
main =  do sls <- shuffleM initCards
           print $ initTexas sls
--           print $ map checkHand $ comb 5 $ head (_holes (initTexas sls)) ++ _community (initTexas sls)
--           print $ map checkHand $ comb 5 $ last (_holes (initTexas sls)) ++ _community (initTexas sls)
           print $ getHighestHand $ head (_hole (initTexas sls)) ++ _community (initTexas sls)
           print $ getHighestHand $ last (_hole (initTexas sls)) ++ _community (initTexas sls)
           putStrLn $ case compare 
                             (fst $ getHighestHand $ head (_hole (initTexas sls)) ++ _community (initTexas sls))
                             (fst $ getHighestHand $ last (_hole (initTexas sls)) ++ _community (initTexas sls))
                             of
                        EQ -> "Check!!"
                        LT -> "Player2 Win!!"
                        GT -> "Player1 Win!!"


           -- Should be [4,32,624,3744,5108,10200,54912,123552,1098240,1302540]

-- main =  do mapM_ (print) $ map (\x -> (x, checkHands x)) $ comb 5 $ initCards
