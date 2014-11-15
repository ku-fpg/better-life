module Main where

-- Libraries for hermit conversion
import Life.Types
import Life.Formations
import Life.Engine.HERMIT.HuttonS			-- Target module for hermit

-- Libraries for testing
import qualified Life.Engine.Set as Set		-- Needed to test correctness with QuickCheck
import Test.QuickCheck 				-- For correctness tests
import Data.List (sort)

import Criterion.Main 				-- For performance tests

-- Runs the Life (without display) for the specified number of generations
life :: Int -> Config -> [Pos] -> Board
life x c = (runLife x) . (scene c)

lifeSet :: Int -> Config -> [Pos] -> Set.Board
lifeSet x c = (runLife x) . (scene c)


-- QuickCheck test of source code engine vs. hermit converted engine
testHermit x c b = sort (alive (life x c b)) == sort (alive (lifeSet x c b))


-- Tests conversion against original for correctness and performance
main :: IO ()
main = do
--    quickCheck $ testHermit 1000 ((20,20),True) glider
--    quickCheck $ testHermit 1000 ((50,50),False) gliderGun
    defaultMain
        [ bench "Glider-20x20" $ whnf (life 10 ((20,20),True)) glider
		, bench "GliderGun-50x50" $ whnf (life 100000 ((50,50),False)) $ gliderGunL (5,5)
		, bench "Acorn-100x100" $ whnf (life 100000 ((100,100),True)) $ acorn (20,20)
		, bench "Battle-100x100" $ whnf (life 100000 ((100,100),False)) $ battle (0,0)
		, bench "GGuns-100x100" $ whnf (life 100000 ((100,100),False)) $ gguns (0,0)
        ]


