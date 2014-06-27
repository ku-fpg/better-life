module NoDisplayLife where

import Life.Types
import Life.Console
import Life.Worlds
import HuttonEngine

-- Runs the Life Engine (no display) for the specified number of generations
-- 	Then it prints the final board
lifeEngine :: Board -> Int -> Board
lifeEngine b 0 = b
lifeEngine b n = lifeEngine (next b) $ n-1

main = lifeEngine (Board ((20,20),True) glider) 500

