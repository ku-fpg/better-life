module Main where

-- Libraries for hermit conversion
import Life.Types
import Life.Scenes
import Life.Engine.HuttonA  -- Target module for hermit

-- Libraries for testing
import qualified Life.Engine.Acc as Acc -- Needed to test correctness with QuickCheck
import Test.QuickCheck -- For correctness tests
--import Criterion.Main -- For performance tests
import qualified Data.Array.Accelerate.CUDA as C

-- Runs the Life (without display) for the specified number of generations
life :: Int -> Config -> [Pos] -> Board
life x c = (runLife x) . (scene c)

lifeAcc :: Int -> Config -> [Pos] -> Acc.Board
lifeAcc x c = (runLife x) . (scene c)


-- QuickCheck test of source code engine vs. hermit converted engine
--testHermit x c b = alive (life x c b) == alive (lifeAcc x c b)


-- Tests conversion against original for correctness and performance
main :: IO ()
main = do print $ life 100 ((20,20),False) glider
    --quickCheck $ testHermit 1000 ((20,20),True) glider
    --quickCheck $ testHermit 1000 ((50,50),False) gliderGun

    {-defaultMain
        [ bench "Hutton-G-20x20" $ nf (board . life 10 ((20,20),False)) glider
        , bench "Hutton-GG-50x50" $ nf (board . life 10 ((50,50),False)) gliderGun
        , bench "Hutton-Acorn-200x200" $ nf (board . life 10 ((200,200),False)) acorn
        ]-}


