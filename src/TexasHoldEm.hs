{-# LANGUAGE TemplateHaskell #-}

module TexasHoldEm where

import Control.Lens
import Poker
import Data.List.Split

data Table = Table { _talon     :: [Card]
                   , _hole     :: [[Card]]
                   , _community :: [Card]
                   } deriving (Show)

makeLenses ''Table

initTable n c =  Table { _talon = c
                       , _hole = replicate n []
                       , _community = [] }

dealHoles     :: Int -> Table -> Table
dealHoles n t =  t { _talon = newT
                   , _hole  = zipWith (++) (t ^. hole) (splitEvery n hcs) }
                 where (hcs, newT) = splitAt (n * length (t ^. hole)) (t ^. talon)

dealCommunity     :: Int -> Table -> Table
dealCommunity n t =  t { _talon = newT
                       , _community = newC }
                     where (newC, newT) = splitAt n (t ^. talon)

initTexas :: [Card] -> Table
initTexas =  dealCommunity 5 . dealHoles 2 . initTable 2
