module SetEngine where

import Life.Types
import Life.Engine.Set
import Life.Worlds

-- Runs the Life (without display) for the specified number of generations
-- 	Then it prints the final board
life :: Int -> Config -> [Pos] -> Board
life n c = (runLife n) . (scene c)

main = life 500 ((20,20),True) glider

