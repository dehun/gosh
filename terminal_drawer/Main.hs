import Gosh.Go
import Gosh.GameLoop


main :: IO ()
main = do
    putStrLn "starting game"
    game_loop BlackPlayer initial_go_state
             
