module Main where

import Life.Types
import Life.Engine.UVector
import Life.Scenes

-- Libraries for testing
import qualified Life.Engine.Hutton as Hutton	-- Needed to test correctness with QuickCheck
import Test.QuickCheck 				-- For correctness tests
import Criterion.Main 				-- For performance tests

-- Runs the Life (without display) for the specified number of generations
life :: Int -> Config -> [Pos] -> Board
life x c = (runLife x) . (scene c)

lifeHutton :: Int -> Config -> [Pos] -> Hutton.Board
lifeHutton x c = (runLife x) . (scene c)


-- QuickCheck test of source code engine vs. hermit converted engine
testHermit x c b = alive (life x c b) == alive (lifeHutton x c b)


-- Tests conversion against original for correctness and performance
main :: IO ()
main = do
	quickCheck $ testHermit 100 ((20,20),True) glider
	quickCheck $ testHermit 100 ((50,50),False) gliderGun
	defaultMain
		[ bench "UVector-G-20x20" $ whnf (life 1000000 ((20,20),True)) glider
		, bench "Hutton-G-20x20" $ whnf (lifeHutton 1000000 ((20,20),True)) glider
		, bench "UVector-GG-50x50" $ whnf (life 1000000 ((50,50),False)) gliderGun
		, bench "HuttonGG-50x50" $ whnf (lifeHutton 1000000 ((50,50),False)) gliderGun
		]


