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


show_affinity :: GoState -> String
show_affinity go =
    let
        board_width = goboard_width $ gostate_board go
        board_height = goboard_height $ gostate_board go
        show_game_row r = do
          rows <- (mapM (\c ->show_game_cell (r, c)) [1..board_width])
          return $ concat rows
        show_game_cell pos = do
          affinity <- (estimate_position_affinity pos go)
          case affinity of
            Just BlackPlayer -> return "b"
            Just WhitePlayer -> return "w"
            Nothing -> return "-"
    in unlines $ fst $ runState (mapM show_game_row [1..board_height]) Data.Map.empty       
