module GoSpec (spec) where

import Test.Hspec
import Gosh.Go

initial_go_state = GoState standard_small_board [] BlackPlayer []
    
put_stone_spec :: Spec
put_stone_spec =
    describe "Gosh.Go.put_stone" $ do
      it "can put stone at empty board" $ do
        put_stone BlackPlayer (1, 1) initial_go_state
          `shouldBe`
           Right (GoState standard_small_board [StoneOnBoard (1, 1) BlackStone] WhitePlayer [initial_go_state])

spec :: Spec
spec = do
  put_stone_spec
                   
                 
