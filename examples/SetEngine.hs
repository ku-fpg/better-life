module SetEngine where

import Life.Types
import Life.Engine
import Life.Engine.Set
import Life.Worlds

-- Runs Life (without display) indefinitely
life :: Config -> [Pos] -> Board
life c = lifeEngine . (scene c)

-- Runs the Life (without display) for the specified number of generations
-- 	Then it prints the final board
runLife :: Config -> [Pos] -> Int -> Board
runLife c = runLifeEngine . (scene c)

main = runLife ((20,20),True) glider 500

