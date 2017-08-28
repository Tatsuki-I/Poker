module Poker where

import Data.List
import System.Random.Shuffle

-- data Card = Card { suit :: Suit
--                  , rank :: Rank
--                  } deriving (Eq, Ord)

data Card = Card Rank Suit
            deriving (Eq, Ord)

instance Show Card where
    show (Card suit rank) = show suit ++ " " ++ show rank
--                                  ++ case rank of
--                                       11 -> "Jack"
--                                       12 -> "Queen"
--                                       13 -> "King"
--                                       14 -> "Ace"
--                                       _  -> show rank

getSuit :: Card -> Suit
getSuit (Card _ suit) =  suit

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
            deriving (Show, Eq, Ord, Enum)

data Suit = Spade
          | Heart
          | Diamond
          | Club
            deriving (Show, Eq, Ord, Enum)

initCards :: [Card]
initCards =  do suit <- [Spade ..]
                rank <- [Two .. Ace]
                return $ Card rank suit

data Hand = StraightFlush
          | FourOfAKind
          | FullHouse
          | Flush
          | Straight
          | ThreeOfAKind
          | TwoPair
          | OnePair
          | HighCards
            deriving (Eq, Ord)

instance Show Hand where
    show hand =  case hand of
                   StraightFlush -> "Straight Flush"
                   FourOfAKind   -> "Four of a Kind"
                   FullHouse     -> "Full House"
                   Flush         -> "Flush"
                   Straight      -> "Straight"
                   ThreeOfAKind  -> "Three of a Kind"
                   TwoPair       -> "Two Pair"
                   OnePair       -> "One Pair"
                   HighCards     -> "High Cards"

checkHands :: [Card] -> Hand
checkHands cards | isStraightFlush cards = StraightFlush
                 | isFourOfAKind   cards = FourOfAKind
                 | isFullHouse     cards = FullHouse
                 | isFlush         cards = Flush
                 | isStraight      cards = Straight
                 | isThreeOfAKind  cards = ThreeOfAKind
                 | isTwoPair       cards = TwoPair
                 | isOnePair       cards = OnePair
                 | otherwise             = HighCards

isStraightFlush :: [Card] -> Bool
isStraightFlush cards =  isStraight cards && isFlush cards

isFourOfAKind :: [Card] -> Bool
isFourOfAKind =  undefined

isFullHouse :: [Card] -> Bool
isFullHouse =  undefined

isFlush       :: [Card] -> Bool
isFlush cards =  all (== head suits) suits
                 where suits :: [Suit]
                       suits =  map getSuit cards

isStraight :: [Card] -> Bool
isStraight cards =  and $ g sorted
                    where f :: Card -> Rank
                          f (Card rank _) =  rank
                          sorted :: [Rank]
                          sorted =  map f cards
                          g :: [Rank] -> [Bool]
                          g [_] = []
                          g (card1 : card2 : cards) =  (succ card1 == card2) : g (card2 : cards)


isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind =  undefined

isTwoPair :: [Card] -> Bool
isTwoPair =  undefined

isOnePair :: [Card] -> Bool
isOnePair =  undefined

getCards         :: Int -> [Card] -> ([Card], [Card])
getCards n cards =  (take n cards, drop n cards)
