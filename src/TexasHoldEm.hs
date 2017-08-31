module TexasHoldEm where

import Poker
import Data.List.Split

data Table = Table { _talon     :: [Card]
                   , _holes     :: [[Card]]
                   , _community :: [Card]
                   } deriving (Show)

initTable n c =  Table { _talon = c
                       , _holes = replicate n []
                       , _community = [] }

dealHoles     :: Int -> Table -> Table
dealHoles n t =  t { _talon = newT
                  , _holes = zipWith (++) (_holes t) (splitEvery n hcs) }
                where (hcs, newT) = splitAt (n * (length $ _holes t)) (_talon t)

dealCommunity     :: Int -> Table -> Table
dealCommunity n t =  t { _talon = newT
                       , _community = newC }
                     where (newC, newT) = splitAt n (_talon t)

initTexas :: [Card] -> Table
initTexas =  dealCommunity 5 . dealHoles 2 . initTable 2
