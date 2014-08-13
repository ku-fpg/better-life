module Main where

-- Libraries for hermit conversion
import Life.Types
import Life.Scenes
import Life.Engine.HuttonS			-- Target module for hermit

-- Libraries for testing
import qualified Life.Engine.Set as Set		-- Needed to test correctness with QuickCheck
import Test.QuickCheck 				-- For correctness tests
import Criterion.Main 				-- For performance tests

-- Runs the Life (without display) for the specified number of generations
life :: Int -> Config -> [Pos] -> Board
life x c = (runLife x) . (scene c)

lifeSet :: Int -> Config -> [Pos] -> Set.Board
lifeSet x c = (runLife x) . (scene c)


-- QuickCheck test of source code engine vs. hermit converted engine
testHermit x c b = alive (life x c b) == alive (lifeSet x c b)


-- Tests conversion against original for correctness and performance
main :: IO ()
main = do
	quickCheck $ testHermit 1000 ((20,20),True) glider
	quickCheck $ testHermit 1000 ((50,50),False) gliderGun
	defaultMain
		[ bench "Set-G-20x20" $ whnf (life 1000000 ((20,20),True)) glider
		, bench "Hutton-G-20x20" $ whnf (lifeSet 1000000 ((20,20),True)) glider
		, bench "Set-GG-50x50" $ whnf (life 1000000 ((50,50),False)) gliderGun
		, bench "Hutton-GG-50x50" $ whnf (lifeSet 1000000 ((50,50),False)) gliderGun
		]


