module Poker where

import Data.List
import Data.Maybe
import Data.Function
import Control.Monad
import System.Random.Shuffle
import Lib

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

data Hands = HighCards
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

data HandRank = HandRank Hands [Int]
                deriving (Eq, Ord, Show)

instance Show Hands where
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

checkHand    :: [Card] -> (Hands, [Card])
checkHand cs |  isRoyalStraightFlush cs = (RoyalStraightFlush, cs)
             |  isStraightFlush      cs = (StraightFlush,      cs)
             |  isFourOfAKind        cs = (FourOfAKind,        cs)
             |  isFullHouse          cs = (FullHouse,          cs)
             |  isFlush              cs = (Flush,              cs)
             |  isStraight           cs = (Straight,           cs)
             |  isThreeOfAKind       cs = (ThreeOfAKind,       cs)
             |  isTwoPair            cs = (TwoPair,            cs)
             |  isOnePair            cs = (OnePair,            cs)
             |  otherwise               = (HighCards,          cs)

getHighestHand :: [Card] -> (HandRank, [Card])
getHighestHand =  maximum . map (uncurry parseHand . checkHand) . comb 5



parseHand      :: Hands -> [Card] -> (HandRank, [Card])
parseHand h cs =  case h of
                    RoyalStraightFlush -> (HandRank h [(fromEnum . getSuit . maximum) cs], ncs)
                    StraightFlush      -> (HandRank h [(getRankEnum . maximum) cs], ncs)
                    FourOfAKind        -> (HandRank h $ f cs, ncs)
                    FullHouse          -> (HandRank h $ f cs, ncs)
                    Flush              -> (HandRank h $ (desc . map getRankEnum) cs, ncs)
                    Straight           -> (HandRank h [(getRankEnum . maximum) cs], ncs)
                    ThreeOfAKind       -> (HandRank h $ (g . f) cs, ncs)
                    TwoPair            -> (HandRank h $ ((reverse . ((:) <$> last <*> init)) . f) cs, ncs)
                    OnePair            -> (HandRank h $ (g . f) cs, ncs)
                    _                  -> (HandRank h $ (desc . map getRankEnum) cs, ncs)
                  where ncs = sort cs
                        sortByLn :: Foldable t => [t a] -> [t a]
                        sortByLn =  sortBy (flip compare `on` length)
                        getRankEnum :: Card -> Int
                        getRankEnum = fromEnum . getRank
                        f :: [Card] -> [Int]
                        f =  map head . sortByLn . group . sort . map getRankEnum
                        desc :: Ord a => [a] -> [a]
                        desc =  sortBy (flip compare)
                        g :: Ord a => [a] -> [a]
                        g =  (:) <$> head <*> (desc . tail)

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

isStraight :: [Card] -> Bool
isStraight =  (`isInfixOf` map fromEnum [Two .. Ace]) . ap f sortByRankEnum
-- isStraight =  (`isInfixOf` (aceAsOne : map fromEnum [Two .. Ace]))
--               . ap f sortByRankEnum
              where toRankLs :: [Card] -> [Rank]
                    toRankLs =  map getRank
                    sortByRankEnum :: [Card] -> [Int]
                    sortByRankEnum =  map fromEnum . sort . toRankLs
                    -- aceAsOne :: Int
                    -- aceAsOne =  (pred . fromEnum) Two
                    minMax :: (Foldable t, Ord b) => t b -> (b, b)
                    minMax =  (,) <$> minimum <*> maximum
                    f    :: [Card] -> [Int] -> [Int]
                    f xs =  if (minMax . toRankLs) xs == (Two, Ace)
                              then init
                              -- then (aceAsOne :) . init
                              else id

isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind =  (== 3) . maximum . pairsLength

isTwoPair :: [Card] -> Bool
isTwoPair =  (== [1, 2, 2]) . sortPairsLength

isOnePair :: [Card] -> Bool
isOnePair =  (== [1, 1, 1, 2]) . sortPairsLength

getCards :: Int -> [Card] -> ([Card], [Card])
getCards =  splitAt
