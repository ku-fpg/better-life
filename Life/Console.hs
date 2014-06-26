module Life.Console where

import Life.Types
import System.Console.ANSI
import Control.Concurrent

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

lifeConsole :: Life board => board -> IO ()
lifeConsole b = do
		cls
		showcells b
		threadDelay (50 * 1000)
		lifeConsole (next b)

