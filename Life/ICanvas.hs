module Life.ICanvas where

import Graphics.Blank
import Data.Text (pack)
import Numeric
import Control.Concurrent

import Life.Types

-- This still needs work, the button does not render correctly due to the new variable sized boards

drawGrid :: Size -> Canvas ()
drawGrid (w,h) = do 
	sequence_ [ lineX x height | x <- [10, 20..width] ]
	sequence_ [ lineY y width | y <- [10, 20..height] ]
	where 
		width = fromIntegral $ 10 * w + 10
		height = fromIntegral $ 10 * h + 10
		lineX x y = do 
			beginPath()
			moveTo(x,0)
			lineTo(x,y)
			lineWidth 1
			strokeStyle $ pack "black"
			closePath()
			stroke()

		lineY y x = do 
			beginPath()
			moveTo(0,y)
			lineTo(x,y)
			lineWidth 1
			strokeStyle $ pack "black"
			closePath()
			stroke()

renderSquare :: Pos -> Canvas ()
renderSquare (x,y) = do 
	beginPath()
	fillStyle $ pack $ '#' : (concat [showHex (255 - (x' `mod` 255)) "", 
		                   '0' : (showHex 0 ""),
		                   showHex (y' `mod` 255) ""])
	rect ((fromIntegral x'), (fromIntegral y'), 8, 8)
	closePath()
	fill()
	where 
		x' = 10 * x
		y' = 10 * y


renderSquares :: [Pos] -> Canvas ()
renderSquares xs = mapM_ renderSquare xs
                
clearDeadCells :: [Pos] -> Canvas ()
clearDeadCells b = sequence_ [ clearRect (10 * fromIntegral x, 10 * fromIntegral y, 8, 8) | (x,y) <- b ]

lifeLoop :: Life board => DeviceContext -> board -> IO ()
lifeLoop dc b = do 
	send dc $ do 
		drawGrid $ fst $ config b
		clearDeadCells $ alive $ diff b b'
		renderSquares $ alive b'
	threadDelay (50 * 500)
	lifeLoop dc b'
	where b' = next b

drawButton :: (Float, Float) -> Canvas ()
drawButton (x,y) = do 
	fillStyle $ pack "red"
	fillRect (x, y, 30, 20)
	fillStyle $ pack "white"
	fillText (pack "start", x + 5, y + 12.5)
                                            
lifeICanvas :: Life board => DeviceContext -> board -> IO ()
lifeICanvas dc b = do 
	send dc $ do 
		drawGrid $ fst $ config b
		drawButton (140, 325)
	event <- wait dc
	case ePageXY event of
	-- if no mouse location, ignore, and redraw
		Nothing -> lifeICanvas dc b
		Just (x',y') -> 
			if floor x' >= 140 && floor x' <= 170 && floor y' >= 325 && floor y' <= 345 
			then lifeLoop dc b
			else if (floor x') `div` 10 > fst (fst (config b)) || (floor y') `div` 10 > snd (fst (config b))
				then lifeICanvas dc b
				else do 
					send dc $ renderSquare ((floor x') `div` 10, (floor y') `div` 10)
					lifeICanvas dc $ inv ((floor x') `div` 10, (floor y') `div` 10) b


