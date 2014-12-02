module Main where

-- Libraries for hermit conversion
import Life.Types
import Life.Scenes
import Life.Formations
import Life.Reader
import Life.Engine.HERMIT.HuttonA  -- Target module for hermit
-- Libraries for testing

import Criterion.Main -- For performance tests
import qualified Data.Array.Accelerate.CUDA as C

-- Runs the Life (without display) for the specified number of generations
life :: Int -> Config -> [Pos] -> Board
life x c = (runLife x) . (scene c)




-- QuickCheck test of source code engine vs. hermit converted engine
--testHermit x c b = alive (life x c b) == alive (lifeAcc x c b)


-- Tests conversion against original for correctness and performance
main :: IO ()
main = do b <- parseLife "Life/test.txt"
          --print $ life 100 ((50,50),False) b
          defaultMain [ bench "Test" $ nf (board . life 100 ((50,50),False)) b]


