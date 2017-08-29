module Poker where

import Data.List
import Data.Maybe
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

data Hand = RoyalStraightFlush
          | StraightFlush
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
    show h =  case h of
                RoyalStraightFlush -> "Royal Straight Flush"
                StraightFlush -> "Straight Flush"
                FourOfAKind   -> "Four of a Kind"
                FullHouse     -> "Full House"
                Flush         -> "Flush"
                Straight      -> "Straight"
                ThreeOfAKind  -> "Three of a Kind"
                TwoPair       -> "Two Pair"
                OnePair       -> "One Pair"
                HighCards     -> "High Cards"

checkHands       :: [Card] -> Hand
checkHands cards |  isRoyalStraightFlush cards = RoyalStraightFlush
                 |  isStraightFlush      cards = StraightFlush
                 |  isFourOfAKind        cards = FourOfAKind
                 |  isFullHouse          cards = FullHouse
                 |  isFlush              cards = Flush
                 |  isStraight           cards = Straight
                 |  isThreeOfAKind       cards = ThreeOfAKind
                 |  isTwoPair            cards = TwoPair
                 |  isOnePair            cards = OnePair
                 |  otherwise                  = HighCards

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
isStraight cs =  (and . f) (if (minimum . toRankLs) cs == Two
                            && (maximum . toRankLs) cs == Ace
                              then (pred . fromEnum) Two
                                   : (init . sortByRankEnum) cs
                              else sortByRankEnum cs)
                 where toRankLs :: [Card] -> [Rank]
                       toRankLs =  map getRank
                       sortByRankEnum :: [Card] -> [Int]
                       sortByRankEnum =  map fromEnum . sort . toRankLs
                       f                :: [Int] -> [Bool]
                       f [_]            =  []
                       f (x1 : x2 : xs) =  (succ x1 == x2) : f (x2 : xs)

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
