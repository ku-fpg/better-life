module Main where

-- Libraries for hermit conversion
import Life.Engine.HERMIT.HuttonV				-- Target module for hermit
import Life.Types
import Life.Formations

-- Libraries for testing
import qualified Life.Engine.UVector as UVector 	-- Needed to test correctness with QuickCheck
import Test.QuickCheck 				-- For correctness tests
import Data.List (sort)

import Criterion.Main 				-- For performance tests

-- Runs the Life (without display) for the specified number of generations
life :: Int -> Config -> Scene -> Board
life x c = (runLife x) . (scene c)

lifeUVector :: Int -> Config -> Scene -> UVector.Board
lifeUVector x c = (runLife x) . (scene c)


-- QuickCheck test of source code engine vs. hermit converted engine
testHermit x c b = sort (alive (life x c b)) == sort (alive (lifeUVector x c b))


-- Tests conversion against original for correctness and performance
main :: IO ()
main = do
{-    quickCheck $ testHermit 1000 ((20,20),True) $ glider (0,0)
    quickCheck $ testHermit 1000 ((50,50),False) $ gliderGunL (0,0)
	quickCheck $ testHermit 1000 ((50,50),False) $ acorn (0,0)
-}
    defaultMain
		[ bench "Glider-20x20" $ whnf (life 1000000 ((20,20),True)) $ glider (0,0)
		, bench "GliderGun-50x50" $ whnf (life 1000000 ((50,50),False)) $ gliderGunL (5,5)
		, bench "Acorn-100x100" $ whnf (life 1000000 ((100,100),True)) $ acorn (20,20)
		, bench "Battle-100x100" $ whnf (life 1000000 ((100,100),False)) $ battle (0,0)
		, bench "Gliders5-160x160" $ whnf (life 1000000 ((160,160),True)) $ gliders5 (0,0)
        ]


