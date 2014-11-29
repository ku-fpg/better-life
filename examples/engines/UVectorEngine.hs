module Main where

import Life.Engine.UVector
import Life.Types
--import Life.Scenes
-- or
import Life.Formations

-- Libraries for testing
import qualified Life.Engine.Hutton as Hutton	-- Needed to test correctness with QuickCheck
import Test.QuickCheck 				-- For correctness tests
import Data.List (sort)

import Criterion.Main 				-- For performance tests

-- Runs the Life (without display) for the specified number of generations
life :: Int -> Config -> Scene -> Board
life x c = (runLife x) . (scene c)

lifeHutton :: Int -> Config -> Scene -> Hutton.Board
lifeHutton x c = (runLife x) . (scene c)


-- QuickCheck test of source code engine vs. hermit converted engine
testHermit x c b = sort (alive (life x c b)) == sort (alive (lifeHutton x c b))


-- Tests conversion against original for correctness and performance
main :: IO ()
main = do
	quickCheck $ testHermit 100 ((20,20),True) $ glider (0,0)
	quickCheck $ testHermit 1000 ((50,50),False) $ gliderGunL (0,0)
	quickCheck $ testHermit 1000 ((50,50),False) $ acorn (0,0)
{-	defaultMain
		[ bench "UVector-G-20x20" $ whnf (life 1000 ((20,20),True)) $ glider (0,0)
		, bench "UVector-GG-50x50" $ whnf (life 1000 ((50,50),False)) $ gliderGunL (0,0)
		, bench "UVector-A-50x50" $ whnf (life 1000 ((50,50),False)) $ acorn (0,0)
		]
-}

