module Gosh.Go where

import Data.List
import qualified Data.Map
import Data.Maybe
import Data.Either
import Control.Monad.State
import qualified Data.Text

    
type Position = (Integer, Integer)    

data GoStone = BlackStone | WhiteStone
               deriving (Show, Eq)

data GoBoard = GoBoard { goboard_width :: Integer
                       , goboard_height :: Integer}
               deriving (Show, Eq)

data StoneOnBoard = StoneOnBoard Position GoStone
                  deriving (Show, Eq)

data GoPlayer = WhitePlayer | BlackPlayer
              deriving (Show, Eq)

data GoState = GoState {
      gostate_board :: GoBoard
    , gostate_stones ::  [StoneOnBoard]
    , gostate_turn :: GoPlayer
    , gostate_previous_states :: [GoState]
      } deriving (Show, Eq)



get_stone_at_position :: GoState -> Position -> Maybe StoneOnBoard
get_stone_at_position go requested_pos =
    let
        stones = gostate_stones go
    in
      case filter (\(StoneOnBoard pos stone) -> pos == requested_pos) stones of
        [] -> Nothing
        [stone] ->  Just stone
        other -> error "more than one stone at one position"


stone_on_board_color :: StoneOnBoard -> GoStone
stone_on_board_color (StoneOnBoard _ color) = color

stone_on_board_position :: StoneOnBoard -> Position
stone_on_board_position (StoneOnBoard pos _) = pos

player_stone :: GoPlayer -> GoStone
player_stone WhitePlayer = WhiteStone
player_stone BlackPlayer = BlackStone

stone_of_player :: GoStone -> GoPlayer
stone_of_player BlackStone = BlackPlayer
stone_of_player WhiteStone = WhitePlayer                             


opposite_player :: GoPlayer -> GoPlayer
opposite_player WhitePlayer = BlackPlayer
opposite_player BlackPlayer = WhitePlayer

opposite_stone :: GoStone -> GoStone
opposite_stone WhiteStone = BlackStone
opposite_stone BlackStone = WhiteStone                            


is_position_on_board :: GoState -> Position -> Bool
is_position_on_board go (row, col) =
    let
        board_width = goboard_width $ gostate_board go
        board_height = goboard_height $ gostate_board go
    in
      (row >= 1)
      && (row <= board_height)
      && (col >= 1 )
      && (col <= board_width)


get_next_positions :: GoState -> Position -> [Position]
get_next_positions go pos =
    let
        possible_positions = map
                             (\(row, col) -> (fst pos + row, snd pos + col))
                             [(1, 0), (-1, 0), (0, 1), (0, -1)]
        next_positions = filter (is_position_on_board go) possible_positions
    in next_positions

stone_neighbours :: GoState -> StoneOnBoard -> [StoneOnBoard]
stone_neighbours go stone =
    map fromJust
            (filter
             (/= Nothing)
             (map (get_stone_at_position go) (get_next_positions go (stone_on_board_position stone))))


stone_group :: GoState -> StoneOnBoard -> [StoneOnBoard]
stone_group go start_stone =
    let
        (StoneOnBoard _ our_stone_type) = start_stone
        expand_stone :: [StoneOnBoard] -> StoneOnBoard -> [StoneOnBoard]
        expand_stone start_path stone =  
            foldl (\path next_stone ->
                       case Data.List.find (== next_stone) path
                       of Just _ -> path
                          Nothing -> expand_stone (next_stone : path) next_stone)
                  start_path
                  (filter
                   (\(StoneOnBoard _ stone_type) -> stone_type == our_stone_type)
                   (stone_neighbours go stone))
    in expand_stone [start_stone] start_stone

                              
single_stone_liberties :: GoState -> StoneOnBoard -> [Position]
single_stone_liberties go start_stone =
    let neighbours = stone_neighbours go start_stone
        next_positions = get_next_positions go (stone_on_board_position start_stone)
    in
      filter (\pos -> case get_stone_at_position go pos
                      of Just _ -> False
                         Nothing -> True) next_positions
      


stone_group_liberties :: GoState -> StoneOnBoard -> [Position]
stone_group_liberties go start_stone =
    concat $ map (single_stone_liberties go) (stone_group go start_stone)


is_free_position :: GoState -> Position -> Bool
is_free_position go pos =
    case get_stone_at_position go pos of
      Just _ -> False
      Nothing -> True


is_group_enclosed :: StoneOnBoard -> GoState -> Bool
is_group_enclosed group_start_stone go = length (stone_group_liberties go group_start_stone) == 0


capture_stones :: StoneOnBoard -> GoState -> (GoState, [StoneOnBoard])
capture_stones stone go =
    let new_stones = stone : (gostate_stones go)
        (StoneOnBoard _ capturing_color) = stone
        capturing_go = GoState (gostate_board go) new_stones (gostate_turn go) (gostate_previous_states go)
        is_group_captured group_start_stone = let (StoneOnBoard _ group_color) = group_start_stone
                                              in group_color /= capturing_color
                                                 && is_group_enclosed group_start_stone capturing_go
        captured_stones = filter is_group_captured new_stones
        left_stones = new_stones \\ captured_stones
        captured_go = GoState (gostate_board go) left_stones (gostate_turn go) (gostate_previous_states go)
    in (captured_go, captured_stones)
      


put_stone :: GoPlayer -> Position -> GoState -> Either String GoState
put_stone player pos go =
    let new_stone = StoneOnBoard pos $ player_stone player
        (captured_go, captured_stones) = capture_stones new_stone go
        new_go = GoState (gostate_board captured_go) (gostate_stones captured_go) (opposite_player player) (go: gostate_previous_states go)
    in
      if player == gostate_turn go
      then
          if (is_free_position go pos)
          then if (length (stone_group_liberties new_go new_stone) > 0)
               then Right new_go
               else Left $ "stone group got no liberties with new stone at " ++ show pos
          else Left $  "place is not free: " ++ (show pos)
      else Left ("not turn of that player " ++ show player)


all_positions :: GoState -> [Position]
all_positions go =
    let
        width = goboard_width $ gostate_board go
        height = goboard_height $ gostate_board go
    in
      concat (map (\row -> map (\col -> (row, col)) [1..width]) [1..height])
