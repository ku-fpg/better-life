module Life.Console where

import System.Console.ANSI
import Control.Concurrent

import Life.Types

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: Pos -> IO ()
goto (x,y) =  putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do 
		goto p
		putStr xs

showcells :: Life board => board -> IO ()
showcells b = sequence_ [ writeat p "O" | p <- alive b ]

-- Runs life with the given board indefinitely
lifeConsole :: Life board => board -> IO ()
lifeConsole b = do
		cls
		showcells b
		threadDelay (50 * 1000)
		lifeConsole (next b)

-- Runs Life with the given board for the given number of generations
-- 	At the end of the run it returns the final board configuration
runLifeConsole :: Life board => board -> Int -> IO board
runLifeConsole b 0 = return b
runLifeConsole b n = do 
		cls
		showcells b
		threadDelay (50 * 1000)
		runLifeConsole (next b) (n-1)
