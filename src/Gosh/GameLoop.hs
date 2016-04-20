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
    

make_a_turn :: GoPlayer -> GoState -> PlayerTurn -> Either String GoGameContinuation
make_a_turn player go turn = do
    case turn of
      PutStoneTurn pos ->
          ContinueGame <$> put_stone player pos go
      PassTurn -> Right $ ContinueGame $ go
      DrawnTurn -> Right $ StopGame go


game_loop :: GoPlayer -> GoState -> IO ()
game_loop player go = do
  case player of
    BlackPlayer -> putStrLn "black player turn"
    WhitePlayer -> putStrLn "white player turn"
  putStrLn $ show_game go
  putStrLn $ show_affinity go $ game_affinity go

  putStrLn "make your turn: [p]ut stone at <y> <x>, p[a]ss, [d]rawn"
  choice <- getLine
  case parse_turn choice of
    Just turn -> do           
      let next_go = make_a_turn player go turn
      case next_go of
        Right (ContinueGame next_go) -> game_loop (opposite_player player) next_go
        Right (StopGame stop_go) -> putStrLn "game ended"
        Left reason -> do
                        putStrLn $ "error happened: " ++ reason
                        game_loop player go
    Nothing -> do
        putStrLn "invalid input"
        game_loop player go
    
