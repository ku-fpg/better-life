module Main where

import Life.Types
import Life.Console
import Life.Worlds
import Life

-- Runs Life indefinitely
life :: Env -> [Pos] -> IO ()
life e b = lifeConsole (sceneWith e b :: Board)

-- Runs Life (with display) for the specified number of generations
-- 	Then it prints the final board configuration as a list of positions
runLife :: Env -> [Pos] -> Int -> IO Board
runLife e b n = runLifeConsole (sceneWith e b :: Board) n

-- Runs the Life Engine (no display) for the specified number of generations
-- 	Then it prints the final board
lifeEngine :: Board -> Int -> Board
lifeEngine b 0 = b
lifeEngine b n = lifeEngine (next b) $ n-1

-- Test functions
s50 = (50,50)

originalLife = life (s50, torusSurface s50) glider

testFSG = life (s50,flatSurface) glider

testTSGG = life (s50,torusSurface s50) gliderGun

testFSGG = life (s50,flatSurface) gliderGun

testTSGrun20 = runLife (s50, torusSurface s50) glider 20

testFSGrun20 = runLife (s50, flatSurface) glider 20

testLifeEngine = lifeEngine (Board (s50,torusSurface s50) glider) 500

-- Main runs the original version of Life (size 20x20 with wrapping edges) starting with the "glider" board

main = originalLife


