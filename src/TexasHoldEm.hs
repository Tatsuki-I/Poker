{-# LANGUAGE TemplateHaskell #-}

module TexasHoldEm where

import Control.Lens
import Poker
import Data.List.Split

data Table = Table { _talon     :: [Card]
                   , _players   :: [Player]
                   -- , _hole      :: [[Card]]
                   , _community :: [Card]
                   } deriving (Show)

data Player = Player { _name    :: String
                     , _chip    :: Chip
                     , _pot     :: Chip
                     , _hole    :: [Card]
                     , _folded  :: Bool
                     , _pos     :: Position
                     , _ranking :: Word
                     } deriving (Show)

type Position = Int
type Chip = Int

makeLenses ''Table
makeLenses ''Player

initTable       :: [Player] -> [Card] -> Table
initTable ps cs =  Table { _talon     = cs
                         , _players   = ps
                         , _community = [] }

initPlayer     :: String -> Player
initPlayer str =  Player { _name    = str
                         , _chip    = 2000
                         , _pot     = 0
                         , _hole    = []
                         , _folded  = False
                         , _pos     = 0
                         , _ranking = 0 }

dealHoles     :: Int -> Table -> Table
dealHoles n t =  t & talon    .~ nt
                   & players  %~ zipWith dealHole (chunksOf n hcs)
                 where (hcs, nt) = splitAt (n * length (t ^. players))
                                           (t ^. talon)

dealHole      :: [Card] -> Player -> Player
dealHole cs p =  p & hole %~ (++ cs)

dealCommunity     :: Int -> Table -> Table
dealCommunity n t =  t & talon     .~ newT 
                       & community .~ newC
                     where (newC, newT) = splitAt n (t ^. talon)

initTexas    :: [Player] -> [Card] -> Table
initTexas ps =  dealCommunity 5 . dealHoles 2 . initTable ps

judge =  undefined
