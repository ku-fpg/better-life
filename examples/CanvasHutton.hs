module CanvasHutton where

import Graphics.Blank

import Life.Types
import Life.Canvas
import Life.Engine.Hutton
import Life.Worlds

-- Runs Life indefinitely
life :: Config -> [Pos] -> IO ()
life c b = blankCanvas 3000 $ \dc -> lifeCanvas dc (scene c b :: Board)

main = life ((50,50),False) gliderGun

-- Test functions
s50 = (50,50)

testG = life (s50,True) glider

testGFlat = life (s50,False) glider

testGGTorus = life (s50,True) gliderGun


