module ConsoleHutton where

import Life.Types
import Life.Console
import Life.Engine.Hutton
import Life.Worlds

-- Runs Life indefinitely
life :: Env -> [Pos] -> IO ()
life e b = lifeConsole (scene e b :: Board)

-- Runs Life (with display) for the specified number of generations
-- 	Then it prints the final board configuration as a list of positions
runLife :: Env -> [Pos] -> Int -> IO Board
runLife e b n = runLifeConsole (scene e b :: Board) n

-- Runs the Life Engine (no display) for the specified number of generations
-- 	Then it prints the final board
lifeEngine :: Board -> Int -> Board
lifeEngine b 0 = b
lifeEngine b n = lifeEngine (next b) $ n-1

-- Runs the original version of Life (size 20x20 with wrapping edges) starting with the "glider" board
originalLife = life ((20,20),True) glider

main = originalLife

-- Test functions
s50 = (50,50)

testG = life (s50,False) glider

testGG = life (s50,True) gliderGun

testGGFlat = life (s50,False) gliderGun

testGrun20 = runLife (s50,True) glider 20

testGGFlatrun20 = runLife (s50,False) gliderGun 20


