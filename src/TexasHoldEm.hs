module TexasHoldEm where

import Poker

data Table = Table { _talon     :: [Card]
                   , _hole      :: [Card]
                   , _community :: [Card]
                   } deriving (Show)

initTable c =  Table { _talon = c
                     , _hole = []
                     , _community = [] }

dealHole     :: Int -> Table -> Table
dealHole n t =  t { _talon = newT
                  , _hole = newH }
                where (newH, newT) = splitAt n (_talon t)

dealCommunity     :: Int -> Table -> Table
dealCommunity n t =  t { _talon = newT
                       , _community = newC }
                     where (newC, newT) = splitAt n (_talon t)

initTexas :: [Card] -> Table
initTexas =  dealCommunity 5 . dealHole 2 . initTable


