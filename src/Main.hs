import Gosh.Go
import Gosh.GameLoop


main :: IO ()
main = do
    putStrLn "starting game"
    let go = GoState (GoBoard 9 9) [] BlackPlayer
    game_loop BlackPlayer go
             
