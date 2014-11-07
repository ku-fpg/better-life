module Main where

-- Libraries for hermit conversion
import Life.Types
import Life.Scenes
import Life.Engine.HuttonV            -- Target module for hermit

-- Libraries for testing
import qualified Life.Engine.UVector as Vector    -- Needed to test correctness with QuickCheck
import Test.QuickCheck                 -- For correctness tests
import Criterion.Main                 -- For performance tests

-- Runs the Life (without display) for the specified number of generations
life :: Int -> Config -> [Pos] -> Board
life x c = (runLife x) . (scene c)

lifeVector :: Int -> Config -> [Pos] -> Vector.Board
lifeVector x c = (runLife x) . (scene c)


-- QuickCheck test of source code engine vs. hermit converted engine
test x c b = alive (life x c b) == alive (lifeVector x c b)


-- Tests conversion against original for correctness and performance
main :: IO ()
main = do
    quickCheck $ test 0 ((20,20),True) glider
    --quickCheck $ test 1000 ((50,50),False) gliderGun
{-    defaultMain
        [ bench "Hutton-G-20x20" $ nf (board . life 10 ((20,20),True)) glider
        , bench "Hutton-GG-50x50" $ nf (board . life 10 ((50,50),False)) gliderGun
        , bench "Hutton-Acorn-200x200" $ nf (board . life 10 ((200,200),False)) acorn
        ]
-}

