module SetConsole where

import Life.Types
import Life.Console
import Life.Engine.Set
import Life.Worlds

-- Runs Life indefinitely
life :: Config -> [Pos] -> IO ()
life c b = lifeConsole (scene c b :: Board)

-- Runs Life for the specified number of generations
-- 	Then it prints the final board configuration as a list of positions
lifeX :: Int -> Config -> [Pos] -> IO Board
lifeX x c b = lifeXConsole x (scene c b :: Board)

-- Runs the original version of Life (size 20x20 with wrapping edges) starting with the "glider" board
originalLife = life ((20,20),True) glider

main = originalLife

-- Test functions
s50 = (50,50)

testG = life (s50,False) glider

testGG = life (s50,True) gliderGun

testGGFlat = life (s50,False) gliderGun

testGrun20 = lifeX 20 (s50,True) glider

testGGFlatrun20 = lifeX 20 (s50,False) gliderGun


