module Gosh.TerminalDrawer where

import Gosh.Go
import Gosh.Scoring

import Data.List
import qualified Data.Map
import Data.Maybe
import Data.Either
import Control.Monad.State
import qualified Data.Text    

    
show_game :: GoState -> String
show_game go =
    let
        board_width = goboard_width $ gostate_board go
        board_height = goboard_height $ gostate_board go
        show_game_row row =
            concat $ map (\col -> show_game_cell (row, col)) [1..board_width]
        show_game_cell pos =
            case get_stone_at_position go pos of
              (Just (StoneOnBoard _ BlackStone)) -> "B"
              (Just (StoneOnBoard _ WhiteStone)) -> "W"
              Nothing -> "-"
    in unlines $  map show_game_row [1..board_height]


show_affinity :: GoState -> Data.Map.Map Position (Maybe GoPlayer) -> String
show_affinity go affinity_mappings =
    let
        board_width = goboard_width $ gostate_board go
        board_height = goboard_height $ gostate_board go
        show_game_row r =
          let rows =  (map (\c ->show_game_cell (r, c)) [1..board_width])
          in concat rows
        show_game_cell pos = 
          case join $ Data.Map.lookup pos affinity_mappings of
            Just BlackPlayer -> "b"
            Just WhitePlayer -> "w"
            Nothing -> "-"
    in unlines $ map show_game_row [1..board_height]
