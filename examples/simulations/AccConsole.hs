module AccConsole where

import Life.Types
import Life.Engine.Acc
import Life.Display.Console
import Life.Scenes
import qualified Data.Array.Accelerate.CUDA as A

-- Runs Life indefinitely
life :: Config -> [Pos] -> IO Board
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

testGrun20 = do res <- lifeX 20 (s50,True) glider
                return $ A.run (board res)

testGGFlatrun20 = lifeX 20 (s50,False) gliderGun


