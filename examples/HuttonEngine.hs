module Main where

import Life.Types
import Life.Engine.Hutton
import Life.Scenes

import Criterion.Main

-- Runs the Life (without display) for the specified number of generations
life :: Int -> Config -> [Pos] -> Board
life x c = (runLife x) . (scene c)

-- It prints the final board after execution
main :: IO ()
main = defaultMain
	[ bench "Glider20x20" $ whnf (life 1000000 ((20,20),True)) glider
	, bench "GliderGun50x50" $ whnf (life 1000000 ((50,50),False)) gliderGun
	]


