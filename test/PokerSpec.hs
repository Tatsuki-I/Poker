module PokerSpec (spec) where

import Poker
import Test.Hspec
import Data.List

spec :: Spec
spec = describe "checkHand" $ do it "standard" $ ((reverse . map length . group . sort . map (fst . checkHand) . comb 5) initCards) `shouldBe` [4,36,624,3744,5108,10200,54912,123552,1098240,1302540]
