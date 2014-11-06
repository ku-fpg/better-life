module Main where

-- Libraries for hermit conversion
import Life.Types
import Life.Scenes
import Life.Engine.Hutton

import Criterion.Main 				-- For performance tests

-- Runs the Life (without display) for the specified number of generations
life :: Int -> Config -> [Pos] -> Board
life x c = (runLife x) . (scene c)

-- Tests conversion against original for correctness and performance
main :: IO ()
main = defaultMain
		[ bench "Glider20x20" $ nf (life 1000 ((20,20),True)) glider
		, bench "GliderGun50x50" $ nf (life 1000 ((50,50),False)) gliderGun
		, bench "Acorn50x50" $ nf (life 1000 ((50,50),False)) acorn
		]


