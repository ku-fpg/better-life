module SetConsole where

import Life.Engine.Set
import Life.Display.Console
import Life.Types
--import Life.Scenes
-- or
import Life.Formations

-- Runs Life indefinitely
life :: Config -> Scene -> IO Board
life c b = lifeConsole (scene c b :: Board)

-- Runs Life for the specified number of generations
-- 	Then it prints the final board configuration as a list of positions
lifeX :: Int -> Config -> Scene -> IO Board
lifeX x c b = lifeXConsole x (scene c b :: Board)

-- Runs the original version of Life (size 20x20 with wrapping edges) starting with the "glider" board
originalLife = life ((20,20),True) $ glider (0,0)

main = originalLife


