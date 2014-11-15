module Main where

import Life.Types
import Life.Engine.Hutton
import Life.Display.Console
--import Life.Scenes
-- or
import Life.Formations

-- Runs Life indefinitely
life :: Config -> Scene -> IO ()
life c s = lifeConsole (scene c s :: Board)

-- Runs Life for the specified number of generations
-- 	Then it prints the final board configuration as a list of positions
lifeX :: Int -> Config -> Scene -> IO ()
lifeX x c s = lifeXConsole x (scene c s :: Board)

-- Runs the original version of Life (size 20x20 with wrapping edges) starting with the "glider" board
-- originalLife = life ((20,20),True) glider

main = life ((40,40),True) $ gliders3 (0,0)


