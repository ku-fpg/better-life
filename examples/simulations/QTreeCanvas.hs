module Main where

import Graphics.Blank

import Life.Types
import Life.Engine.Hutton
import Life.Display.Canvas
import Life.Scenes

-- Runs Life indefinitely
life :: Config -> [Pos] -> IO ()
life c b = blankCanvas 3000 $ \dc -> lifeCanvas dc (scene c b :: Board)

main = life ((50,50),False) gliderGun

-- Test functions
s50 = (50,50)

testG = life (s50,True) glider

testGFlat = life (s50,False) glider

testGGTorus = life (s50,True) gliderGun


