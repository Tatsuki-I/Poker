module Poker where

import Data.List
import System.Random.Shuffle

data Card = Card Rank Suit
            deriving (Eq, Ord)

instance Show Card where
    show (Card suit rank) = show suit ++ " " ++ show rank

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
            deriving (Eq, Ord, Enum)

instance Show Rank where
    show r = case r of
               Jack  -> "J"
               Queen -> "Q"
               King  -> "K"
               Ace   -> "A"
               _     -> (show . (+ 2) . fromEnum) r

data Suit = Spade
          | Heart
          | Diamond
          | Club
            deriving (Eq, Ord, Enum)

instance Show Suit where
    show s = case s of
               Spade   -> "♠ "
               Heart   -> "♥ "
               Diamond -> "♦ "
               Club    -> "♣ "

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
isFourOfAKind cards =  (sort . map (length) . pairs) cards == [1, 4]

isFullHouse :: [Card] -> Bool
isFullHouse cards =  (sort . map (length) . pairs) cards == [2, 3]

isFlush       :: [Card] -> Bool
isFlush cards =  all (== head suits) suits
                 where suits :: [Suit]
                       suits =  map getSuit cards

isStraight :: [Card] -> Bool
isStraight cards =  and $ g sorted
                    where f :: Card -> Rank
                          f (Card rank _) =  rank
                          sorted :: [Rank]
                          sorted =  sort $ map f cards
                          g :: [Rank] -> [Bool]
                          g [_] = []
                          g (card1 : card2 : cards) =  (succ card1 == card2) : g (card2 : cards)

pairs cards =  group sorted
               where f (Card rank _) =  rank
                     sorted =  sort $ map f cards

isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind cards =  (last . sort . map (length) . pairs) cards == 3

isTwoPair :: [Card] -> Bool
isTwoPair cards =  (sort . map (length) . pairs) cards == [1, 2, 2]

isOnePair :: [Card] -> Bool
isOnePair cards =  (sort . map (length) . pairs) cards == [1, 1, 1, 2]

getCards         :: Int -> [Card] -> ([Card], [Card])
getCards n cards =  (take n cards, drop n cards)

