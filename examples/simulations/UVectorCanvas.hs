module Main where

import Graphics.Blank

import Life.Engine.UVector
import Life.Display.Canvas
import Life.Types
import Life.Formations

-- Runs Life indefinitely
life :: Config -> Scene -> IO ()
life c b = blankCanvas 3000 $ \dc -> lifeCanvas dc (scene c b :: Board)

-- Runs Life for the specified number of generations
lifeX :: Int -> Config -> Scene -> IO ()
lifeX x c s = blankCanvas 3000 $ \dc -> lifeXCanvas x dc (scene c s :: Board)

main = life ((160,160),True) $ gliders3 (0,0)


