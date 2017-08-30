module PokerSpec (spec) where

import Poker
import Test.Hspec
import Data.List

spec :: Spec
spec = describe "checkHands" $ do it "standard" $ ((reverse . map length . group . sort . map (snd . checkHands) . comb 5) initCards) `shouldBe` [4,36,624,3744,5108,10200,54912,123552,1098240,1302540]
