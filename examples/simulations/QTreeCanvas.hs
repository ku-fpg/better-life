module Main where

import Graphics.Blank

import Life.Engine.Hutton
import Life.Display.Canvas
import Life.Types
--import Life.Scenes
-- or
import Life.Formations

-- Runs Life indefinitely
life :: Config -> [Pos] -> IO ()
life c b = blankCanvas 3000 $ \dc -> lifeCanvas dc (scene c b :: Board)

-- Runs Life for the specified number of generations
-- 	Then it prints the final board configuration as a list of positions
lifeX :: Int -> Config -> Scene -> IO ()
lifeX x c s = blankCanvas 3000 $ \dc -> lifeXCanvas x dc (scene c s :: Board)

main = life ((50,50),False) $ gliderGun (0,0)


