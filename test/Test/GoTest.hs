module Test.GoTest where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Gosh.Go


initial_state = GoState standard_small_board [] BlackPlayer []

goSuit :: Test
goSuit = testGroup "Go logic tests"
         [ testCase "put single stone on empty board" (testPutSingleStone)
         , testCase "put stone into already occypied position" (testPutStoneAlreadyOccupied) ]

testPutSingleStone :: Assertion
testPutSingleStone =
    put_stone BlackPlayer (1, 1) initial_state
    @=? Right (GoState standard_small_board [StoneOnBoard (1, 1) BlackStone] WhitePlayer [initial_state])

testPutStoneAlreadyOccupied :: Assertion
testPutStoneAlreadyOccupied =
    let go = (GoState standard_small_board [StoneOnBoard (1, 1) BlackStone] WhitePlayer [initial_state])
    in put_stone WhitePlayer (1, 1) go @=? Left "place is not free: (1,1)"
