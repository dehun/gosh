module GoSpec (spec) where

import Test.Hspec
import Gosh.Go
import qualified Data.HashSet

put_stone_spec :: Spec
put_stone_spec =
    describe "Gosh.Go.put_stone" $ do
      it "can put stone at empty board" $ do
        put_stone BlackPlayer (1, 1) initial_go_state
          `shouldBe`
           Right (GoState standard_small_board
                          (Data.HashSet.fromList [StoneOnBoard (1, 1) BlackStone])
                          WhitePlayer [initial_go_state])
      it "can put stone at empty board in the border" $ do
        put_stone BlackPlayer (9, 9) initial_go_state
          `shouldBe`
           Right (GoState standard_small_board
                          (Data.HashSet.fromList [StoneOnBoard (9, 9) BlackStone])
                          WhitePlayer [initial_go_state])
      it "should not allow to put stone out of the board" $ do
        put_stone BlackPlayer (20, 20) initial_go_state
          `shouldBe`
           Left "position is not on board: (20,20)"
      it "should not allow to put stone if its not your turn" $ do
        put_stone WhitePlayer (1, 1) initial_go_state
          `shouldBe`
           Left "not turn of that player WhitePlayer"
      it "should capture cornered stone" $ do
        let start_go = GoState standard_small_board
                       (Data.HashSet.fromList [ StoneOnBoard (1, 1) WhiteStone
                                             , StoneOnBoard (1, 2) BlackStone])
                       BlackPlayer []
        put_stone BlackPlayer (2, 1) start_go
          `shouldBe`
           Right (GoState standard_small_board
                          (Data.HashSet.fromList [ StoneOnBoard (2, 1) BlackStone
                                                 , StoneOnBoard (1, 2) BlackStone ])
                          WhitePlayer [start_go])
      it "should capture surrounded stone" $ do
        let start_go = GoState standard_small_board
                       (Data.HashSet.fromList
                            [ StoneOnBoard (2, 3) BlackStone
                            , StoneOnBoard (3, 2) BlackStone, StoneOnBoard (3, 3) WhiteStone
                            , StoneOnBoard (4, 3) BlackStone])
                       BlackPlayer []
        put_stone BlackPlayer (3, 4) start_go
          `shouldBe`
           Right (GoState standard_small_board
                          (Data.HashSet.fromList
                              [ StoneOnBoard (3, 4) BlackStone
                              , StoneOnBoard (2, 3) BlackStone
                              , StoneOnBoard (3, 2) BlackStone
                              , StoneOnBoard (4, 3) BlackStone])
                          WhitePlayer [start_go])
      it "should not repeat position" $ do
        -- -bw-
        -- bw-w
        -- -bw-
        let start_go =
                GoState standard_small_board
                        (Data.HashSet.fromList
                             [ StoneOnBoard (2, 3) BlackStone, StoneOnBoard (2, 4) WhiteStone
                             , StoneOnBoard (3, 2) BlackStone, StoneOnBoard (3, 3) WhiteStone, StoneOnBoard (3, 5) WhiteStone
                             , StoneOnBoard (4, 3) BlackStone, StoneOnBoard (4, 4) WhiteStone])
                       BlackPlayer []
        -- -bw-
        -- b-bw
        -- -bw-                       
        let next_go =
                GoState standard_small_board
                        (Data.HashSet.fromList
                             [ StoneOnBoard (2, 3) BlackStone, StoneOnBoard (2, 4) WhiteStone
                             , StoneOnBoard (3, 2) BlackStone, StoneOnBoard (3, 4) BlackStone, StoneOnBoard (3, 5) WhiteStone
                             , StoneOnBoard (4, 3) BlackStone, StoneOnBoard (4, 4) WhiteStone])
                       WhitePlayer [start_go]                       
        put_stone WhitePlayer (3, 3) next_go
          `shouldBe`
           Left "move repeats previous state"

spec :: Spec
spec = do
  put_stone_spec
                   
                 
