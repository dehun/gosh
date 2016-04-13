module Gosh.Scoring where

import Gosh.Go
import Data.List
import qualified Data.Map
import Data.Maybe
import Data.Either
import Control.Monad.State
import qualified Data.Text
    

get_surrounding_stones :: Position -> GoState -> ([StoneOnBoard], [Position])
get_surrounding_stones start_pos go =
    let
        inner_get_surrounding_stones :: Position -> GoState -> State [Position] [StoneOnBoard] 
        inner_get_surrounding_stones pos go = do
            let surrouding_positions = get_next_positions go pos
            expanded <- get
            if find (==pos) expanded == Nothing
            then do
                put (pos:expanded)
                case get_stone_at_position go pos of
                  Just stone -> return [stone]
                  Nothing -> foldM (\s p -> do
                                      stones <- inner_get_surrounding_stones p go
                                      return (stones ++ s)) [] surrouding_positions
            else return []
    in
      runState (inner_get_surrounding_stones start_pos go) []


estimate_position_affinity :: Position -> GoState
                           -> State (Data.Map.Map Position (Maybe GoPlayer)) (Maybe GoPlayer)
estimate_position_affinity pos go = do
  cache <- get
  case Data.Map.lookup pos cache of
    Just affinity -> return affinity
    Nothing -> do
        let (surrounding_stones, surrounded_area) = get_surrounding_stones pos go
        if length surrounding_stones < 1
        then return Nothing
        else
            if all
                   (\(StoneOnBoard _ color) -> color == stone_on_board_color (head surrounding_stones))
                   surrounding_stones
            then do
              let affinity = Just (stone_of_player $ (stone_on_board_color (head surrounding_stones)))
              mapM_ (\p -> do
                       old_cache <- get
                       put (Data.Map.insert p affinity old_cache)) surrounded_area
              return $ affinity
            else return Nothing
    
         
estimated_player_area :: GoPlayer -> GoState -> [Position]
estimated_player_area player go =
    fst $ runState (filterM (\p -> do
                               affinity <- estimate_position_affinity p go
                               return (Just player == affinity)) (all_positions go)) Data.Map.empty


score :: GoPlayer -> GoState -> Integer
score player go = toInteger $ length $ estimated_player_area player go
