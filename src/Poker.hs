module Poker where

import Data.List
import Data.Maybe
import Control.Monad
import System.Random.Shuffle

data Card = Card Rank Suit
            deriving (Eq, Ord)

instance Show Card where
    show (Card suit rank) = show suit ++ " " ++ show rank

instance Enum Card where
    fromEnum c = fromMaybe 0 $ elemIndex c (sort initCards)
    toEnum     = ((!!) . sort) initCards

getSuit            :: Card -> Suit
getSuit (Card _ s) =  s

data Rank = Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          | Ace
            deriving (Eq, Ord, Enum)

getRank            :: Card -> Rank
getRank (Card r _) =  r

instance Show Rank where
    show r = case r of
               Jack  -> "J"
               Queen -> "Q"
               King  -> "K"
               Ace   -> "A"
               Ten   -> "T"
               _     -> (show . (+ 2) . fromEnum) r

data Suit = Club
          | Diamond
          | Heart
          | Spade
            deriving (Eq, Ord, Enum)

instance Show Suit where
    show s = case s of
               Club    -> "♣ "
               Diamond -> "♢ "
               Heart   -> "♡ "
               Spade   -> "♠ "

initCards :: [Card]
initCards =  do s <- [Club ..]
                r <- [Two .. Ace]
                return $ Card r s

data Hand = HighCards
          | OnePair
          | TwoPair
          | ThreeOfAKind
          | Straight
          | Flush
          | FullHouse
          | FourOfAKind
          | StraightFlush
          | RoyalStraightFlush
            deriving (Eq, Ord, Enum)

instance Show Hand where
    show h =  case h of
                HighCards          -> "High Cards"
                OnePair            -> "One Pair"
                TwoPair            -> "Two Pair"
                ThreeOfAKind       -> "Three of a Kind"
                Straight           -> "Straight"
                Flush              -> "Flush"
                FullHouse          -> "Full House"
                FourOfAKind        -> "Four of a Kind"
                StraightFlush      -> "Straight Flush"
                RoyalStraightFlush -> "Royal Straight Flush"

checkHands   :: [Card] -> ([Card], Hand)
checkHands cs |  isRoyalStraightFlush cs = (cs, RoyalStraightFlush)
              |  isStraightFlush      cs = (cs, StraightFlush)
              |  isFourOfAKind        cs = (cs, FourOfAKind)
              |  isFullHouse          cs = (cs, FullHouse)
              |  isFlush              cs = (cs, Flush)
              |  isStraight           cs = (cs, Straight)
              |  isThreeOfAKind       cs = (cs, ThreeOfAKind)
              |  isTwoPair            cs = (cs, TwoPair)
              |  isOnePair            cs = (cs, OnePair)
              |  otherwise               = (cs, HighCards)

isRoyalStraightFlush :: [Card] -> Bool
isRoyalStraightFlush =  (&&) <$> isStraightFlush
                             <*> (== Ten) . minimum . map getRank

isStraightFlush :: [Card] -> Bool
isStraightFlush =  (&&) <$> isStraight
                        <*> isFlush

pairs :: [Card] -> [[Rank]]
pairs =  group . sort . map getRank

sortPairsLength :: [Card] -> [Int]
sortPairsLength = sort . pairsLength

pairsLength :: [Card] -> [Int]
pairsLength = map length . pairs

isFourOfAKind :: [Card] -> Bool
isFourOfAKind =  (== [1, 4]) . sortPairsLength

isFullHouse :: [Card] -> Bool
isFullHouse =  (== [2, 3]) . sortPairsLength

isFlush :: [Card] -> Bool
isFlush =  all =<< (. getSuit) . (==) . head . map getSuit
-- isFlush cards =  all ((== head (map getSuit cards)) . getSuit) cards

isStraight    :: [Card] -> Bool
isStraight cs =  f cs (sortByRankEnum cs) `isInfixOf`
                 (aceAsOne : map fromEnum [Two .. Ace])
                 where toRankLs :: [Card] -> [Rank]
                       toRankLs =  map getRank
                       sortByRankEnum :: [Card] -> [Int]
                       sortByRankEnum =  map fromEnum . sort . toRankLs
                       aceAsOne = (pred . fromEnum) Two
                       minMax :: (Foldable t, Ord b) => t b -> (b, b)
                       minMax =  (,) <$> minimum
                                     <*> maximum
                       f    :: [Card] -> [Int] -> [Int]
                       f xs =  if (minMax . toRankLs) xs == (Two, Ace)
                                 then (aceAsOne :) . init
                                 else id

isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind =  (== 3) . maximum . pairsLength

isTwoPair :: [Card] -> Bool
isTwoPair =  (== [1, 2, 2]) . sortPairsLength

isOnePair :: [Card] -> Bool
isOnePair =  (== [1, 1, 1, 2]) . sortPairsLength

getCards :: Int -> [Card] -> ([Card], [Card])
getCards =  splitAt

comb          :: Int -> [a] -> [[a]]
comb 0 _        =  [[]]
comb _ []       =  []
comb m (x : xs) =  map (x :) (comb (m - 1) xs) ++ comb m xs
