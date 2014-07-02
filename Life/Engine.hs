module Life.Engine where

import Life.Types

-- Runs life with the given board indefinitely
lifeEngine :: Life board => board -> board
lifeEngine b = lifeEngine $ next b

-- Runs Life with the given board for the given number of generations
-- 	At the end of the run it returns the final board configuration
runLifeEngine :: Life board => board -> Int -> board
runLifeEngine b 0 = b
runLifeEngine b n = runLifeEngine (next b) (n-1)
