module Gosh.GameLoop where

import Gosh.Go
import Gosh.Scoring
import Gosh.TerminalDrawer
    
import Data.List
import qualified Data.Map
import Data.Maybe
import Data.Either
import Control.Monad.State
import qualified Data.Text

data GoGameContinuation = ContinueGame GoState
                        | StopGame GoState
                          deriving (Show, Eq)

data PlayerTurn = PassTurn
                | PutStoneTurn Position
                | DrawnTurn
                  deriving (Show, Eq)

 
parse_position :: String -> Maybe Position                          
parse_position input =
    let coords = map (\s -> (read (Data.Text.unpack s)) :: Integer)
                 (filter (/=Data.Text.empty) (Data.Text.split (==' ')
                                                  (Data.Text.pack (tail input))))
    in case coords of
         (row: col: []) -> return (row, col)
         other -> Nothing


parse_turn :: String -> Maybe PlayerTurn
parse_turn input =
    let headm [] = Nothing
        headm (x:_) = Just x
    in do
      first_char <- headm input
      case first_char of
         'p' -> do
           pos <- parse_position $ tail input
           return $ PutStoneTurn pos
         'a' -> return $ PassTurn
         'd' -> return $ DrawnTurn
    

make_a_turn :: GoPlayer -> GoState -> IO GoGameContinuation
make_a_turn player go = do
    putStrLn "make your turn: [p]ut stone at <y> <x>, p[a]ss, [d]rawn"
    choice <- getLine
    case parse_turn choice of
      Nothing -> do
          putStrLn "cant parse input. try more"
          make_a_turn player go
      Just (PutStoneTurn pos) ->
          do case put_stone player pos go of
               Left reason -> do
                     putStrLn $ "invalid turn : " ++ reason
                     putStrLn "try more"
                     make_a_turn player go
               Right next_go -> return $ ContinueGame $ next_go
      Just PassTurn -> return $ ContinueGame $ go
      Just DrawnTurn -> return $ StopGame go


game_loop :: GoPlayer -> GoState -> IO ()
game_loop player go = do
  case player of
    BlackPlayer -> putStrLn "black player turn"
    WhitePlayer -> putStrLn "white player turn"
  putStrLn $ show_game go
  putStrLn $ show_affinity go $ game_affinity go
  next_go <- make_a_turn player go
  case next_go of
    ContinueGame next_go -> game_loop (opposite_player player) next_go
    StopGame stop_go ->
        do
          putStrLn "game ended"
    
