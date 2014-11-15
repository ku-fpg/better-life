module Main where

import Life.Engine.Hutton
import Life.Types
--import Life.Scenes
-- or
import Life.Formations

import Criterion.Main 				-- For performance tests

-- Runs the Life (without display) for the specified number of generations
life :: Int -> Config -> Scene -> Board
life x c = (runLife x) . (scene c)

-- Tests conversion against original for correctness and performance
main :: IO ()
main = do 
	defaultMain
		[ bench "Gliders3-50x50" $ whnf (life 100000 ((50,50),True)) $ gliders3 (0,0)
		, bench "GliderGun-50x50" $ whnf (life 100000 ((50,50),False)) $ gliderGunL (5,5)
		, bench "Acorn-100x100" $ whnf (life 100000 ((100,100),True)) $ acorn (20,20)
		, bench "Battle-100x100" $ whnf (life 100000 ((100,100),False)) $ battle (0,0)
		, bench "GGuns-100x100" $ whnf (life 100000 ((100,100),False)) $ gguns (0,0)
		]


