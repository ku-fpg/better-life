module Life.Canvas where

import Data.Text (pack)
import Graphics.Blank
import Control.Concurrent
import Numeric

import Life.Types

renderBall :: Pos -> Canvas ()
renderBall (x,y) = do 
	beginPath()
	let x' = 10 * x
	let y' = 10 * y
	fillStyle $ pack $ '#' : concat 
		[showHex (255 - (x' `mod` 255)) "", 
		  '0' : (showHex 0 ""),
		  showHex (y' `mod` 255) ""]
	arc(fromIntegral x', fromIntegral y', 5 , 0, pi*2, False)
	closePath()
	fill()

renderBalls :: [Pos] -> Canvas ()
renderBalls xs = mapM_ renderBall xs

lifeCanvas :: Life board => DeviceContext -> board -> IO ()
lifeCanvas c b = do 
	send c $ do 
		clearRect (0, 0, width c, height c)
		renderBalls $ alive b
	threadDelay $ 50 * 50
	lifeCanvas c $ next b


