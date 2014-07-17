module Life.Display.ICanvas where

import Graphics.Blank
import Data.Text (pack)
import Numeric
import Control.Concurrent

import Life.Types

-- This still needs work, it does not render correctly due to the new variable-size boards

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
	fillStyle $ pack $ "red" 
	{-Need to fix this, some cells are appearing white 
	'#' : (concat [showHex (200 - ((x' + 50) `mod` 200)) "", 
		                              showHex ((x' + y') `mod` 200) "",
		                              showHex ((y' + 50) `mod` 200) ""])
	-}
	rect ((fromIntegral x'), (fromIntegral y'), 9, 9)
	closePath()
	fill()
	where 
		x' = 10 * x
		y' = 10 * y


renderSquares :: [Pos] -> Canvas ()
renderSquares xs = mapM_ renderSquare xs
                
clearDeadCells :: [Pos] -> Canvas ()
clearDeadCells b = sequence_ [ clearRect (10 * fromIntegral x, 10 * fromIntegral y, 9, 9) | (x,y) <- b ]

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
	fillText (pack "start", x + 2.5, y + 12.5)
                                            
lifeICanvas :: Life board => DeviceContext -> board -> IO ()
lifeICanvas dc b = do 
	send dc $ do
		let size = fst $ config b
		drawGrid size
		let bcoords = (5 * (fromIntegral (fst size)) - 15  , 10 * fromIntegral (snd size) + 20 )
		drawButton bcoords
	event <- wait dc
	case ePageXY event of
	-- if no mouse location, ignore, and redraw
		Nothing -> lifeICanvas dc b
		Just (x',y') -> 
		    let (bx,by) = fst $ config b
		        (butx,buty) = (5 * (fromIntegral bx) - 15  , 10 * (fromIntegral by) + 20 ) 
		    in
			    if floor x' >= butx && floor x' <= (butx + 30) && floor y' >= buty && floor y' <= (buty + 20) 
			    then lifeLoop dc b
			    else if (floor x') `div` 10 > bx || (floor y') `div` 10 > by
				    then lifeICanvas dc b
				    else do 
					    send dc $ renderSquare ((floor x') `div` 10, (floor y') `div` 10)
					    lifeICanvas dc $ inv ((floor x') `div` 10, (floor y') `div` 10) b


