module Gosh.GameLoop where

import Gosh.Go
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
    

make_a_turn :: GoPlayer -> GoState -> IO GoGameContinuation
make_a_turn player go = do
    putStrLn "make your turn: [p]ut stone at <y> <x>, p[a]ss, [d]rawn"
    choice <- getLine
    if length choice == 0
    then do
      putStrLn "invalid input"
      make_a_turn player go
    else do
      case head choice of
        'p' -> do
           let parse_position input = map (\s -> (read (Data.Text.unpack s)) :: Integer)
                                      (filter (/=Data.Text.empty) (Data.Text.split (==' ')
                                                                       (Data.Text.pack (tail input))))
           let (row: col: []) =  parse_position choice
           case put_stone player (row, col) go of
                Left reason -> do
                    putStrLn $ "invalid turn : " ++ reason
                    putStrLn "try more"
                    make_a_turn player go
                Right next_go -> return $ ContinueGame $ next_go
        'a' -> return $ ContinueGame $ GoState (gostate_board go) (gostate_stones go) (opposite_player player)
        'd' -> return $ StopGame go
        other -> do
           putStrLn "invalid input. try more"
           make_a_turn player go


game_loop :: GoPlayer -> GoState -> IO ()
game_loop player go = do
  case player of
    BlackPlayer -> putStrLn "black player turn"
    WhitePlayer -> putStrLn "white player turn"
  putStrLn $ show_game go
  putStrLn $ show_affinity go
  next_go <- make_a_turn player go
  case next_go of
    ContinueGame next_go -> game_loop (opposite_player player) next_go
    StopGame stop_go ->
        do
          putStrLn "game ended"
    
