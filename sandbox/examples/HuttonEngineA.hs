module Main where

-- Libraries for hermit conversion
import Life.Types
import Life.Scenes
import Life.Engine.HuttonA			-- Target module for hermit

-- Libraries for testing
import qualified Life.Engine.Acc as Acc -- Needed to test correctness with QuickCheck
import Test.QuickCheck -- For correctness tests
import Criterion.Main -- For performance tests

-- Runs the Life (without display) for the specified number of generations
life :: Int -> Config -> [Pos] -> Board
life x c = (runLife x) . (scene c)

lifeAcc :: Int -> Config -> [Pos] -> Acc.Board
lifeAcc x c = (runLife x) . (scene c)


-- QuickCheck test of source code engine vs. hermit converted engine
testHermit x c b = alive (life x c b) == alive (lifeAcc x c b)


-- Tests conversion against original for correctness and performance
main :: IO ()
main = do
	quickCheck $ testHermit 1000 ((20,20),True) glider
	quickCheck $ testHermit 1000 ((50,50),False) gliderGun
	defaultMain
		[ bench "Acc-G-20x20" $ whnf (life 1000000 ((20,20),True)) glider
		, bench "Hutton-G-20x20" $ whnf (lifeAcc 1000000 ((20,20),True)) glider
		, bench "Acc-GG-50x50" $ whnf (life 1000000 ((50,50),False)) gliderGun
		, bench "Hutton-GG-50x50" $ whnf (lifeAcc 1000000 ((50,50),False)) gliderGun
		]


