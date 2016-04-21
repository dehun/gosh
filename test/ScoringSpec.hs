module ScoringSpec (spec) where

import Test.Hspec
import Gosh.Go
import Gosh.Scoring    
import qualified Data.HashSet

scoring_spec :: Spec
scoring_spec =
    describe "Gosh.Scoring.score" $ do
      it "white score for empty board" $ do
        score WhitePlayer initial_go_state `shouldBe` 0
      it "black score for empty board" $ do
        score BlackPlayer initial_go_state `shouldBe` 0
      let few_stones_state = GoState
                        standard_small_board
                        (Data.HashSet.fromList
                             [StoneOnBoard (1, 1) BlackStone,
                              StoneOnBoard (1, 2) BlackStone,                                             
                              StoneOnBoard (2, 1) WhiteStone])
                        BlackPlayer
                        []              
      it "black score for few stones" $ do
        score BlackPlayer few_stones_state  `shouldBe` 2
      it "white score for few stones" $ do
        score WhitePlayer few_stones_state  `shouldBe` 1


spec :: Spec
spec = do
  scoring_spec                
