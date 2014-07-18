module ArrayEngine where

import Life.Types
import Life.Engine.Array
import Life.Scenes

-- Runs the Life (without display) for the specified number of generations
life :: Int -> Config -> [Pos] -> Board
life x c = (runLife x) . (scene c)

-- It prints the final board after execution
main :: IO ()
main = print $ life 500 ((20,20),True) glider

