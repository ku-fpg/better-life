module Main where

import Graphics.Blank

import Life.Types
import Life.Engine.Hutton
import Life.Display.Canvas
import Life.Formations

-- Runs Life indefinitely
life :: Config -> Scene -> IO ()
life c b = blankCanvas 3000 $ \dc -> lifeCanvas dc (scene c b :: Board)

-- Runs Life for the specified number of generations
-- 	Then it prints the final board configuration as a list of positions
lifeX :: Int -> Config -> Scene -> IO ()
lifeX x c s = blankCanvas 3000 $ \dc -> lifeXCanvas x dc (scene c s :: Board)

main = life ((100,100),False) $ battle (0,0)


