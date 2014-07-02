{-# LANGUAGE MultiWayIf #-}

import Graphics.Blank
import LifeUnwrapped
import Numeric
import Control.Concurrent
import LifeIO


renderSquare :: (Int,Int)  -> Canvas ()
renderSquare (x,y) = do beginPath()
                        let x' = 10 * x
                        let y' = 10 * y
                        fillStyle $ '#' : (concat [showHex (255 - (x' `mod` 255)) "", 
                                                   '0' : (showHex 0 ""),
                                                   showHex (y' `mod` 255) ""])
                        rect ((fromIntegral x') + 1, (fromIntegral y') + 1, 8, 8)
                        closePath()
                        fill()

renderSquares :: [(Int, Int)] -> Canvas ()
renderSquares xs   = mapM_ renderSquare xs
                
drawGrid :: Context -> Canvas ()
drawGrid context = do (width, height) <- size
                      sequence_ [lineX x (10 * (fromIntegral LifeUnwrapped.width) + 10) | x <- [10, 20..(10 * (fromIntegral LifeUnwrapped.width) + 10)]]
                      sequence_ [lineY y (10 * (fromIntegral LifeUnwrapped.width) + 10) | y <- [10, 20..(10 * (fromIntegral LifeUnwrapped.height) + 10)]]
                      where lineX x h = do beginPath()
                                           moveTo(x,0)
                                           lineTo(x,h)
                                           lineWidth 1
                                           strokeStyle "black"
                                           closePath()
                                           stroke()
                     
                            lineY y w = do beginPath()
                                           moveTo(0,y)
                                           lineTo(w,y)
                                           lineWidth 1
                                           strokeStyle "black"
                                           closePath()
                                           stroke() 
loop :: Context -> [(Int, Int)] ->  IO ()                                           
loop context xs = do send context $ 
                          do drawGrid context
                             drawButton context (140, 325)
                     event <- wait context
                     case ePageXY event of
                        -- if no mouse location, ignore, and redraw
                        Nothing -> loop context xs
                        Just (x',y') -> if | floor x' >= 140 && floor x' <= 170 && floor y' >= 325 && floor y' <= 345 -> life context xs
                                           | (floor x') `div` 10 > LifeUnwrapped.width || (floor y') `div` 10 > LifeUnwrapped.height -> loop context xs
                                           | otherwise -> do send context $ renderSquare ((floor x') `div` 10,(floor y') `div` 10)
                                                             loop context (((floor x') `div` 10, (floor y') `div` 10) : xs)   
                                                                               

glider                        :: Board
glider                        =  [(4,2),(2,3),(4,3),(3,4),(4,4)]

glidergun                     :: Board
glidergun                     = [(2,6), (3,6), (2,7), (3,7), (14,4), (15,4), (17,5), (18,6), (18,7), (18,8), (19,7), (17,9), (16,7), (15,10), (14,10), (13,9),
                                 (12,8), (12,7), (12,6), (13,5), (22,4), (22,5), (22,6), (23, 4), (23,5), (23,6), (24,3), (24,7), (26,2), (26,3), (26,7), (26,8),
                                 (36,4), (36,5), (37,4), (37,5)]

clearDeadCells :: [(Int, Int)] -> Canvas ()
clearDeadCells xs = sequence_ [clearRect (10 * (fromIntegral x) + 1, 10 * (fromIntegral y) + 1, 8, 8 )| (x,y) <- xs]
                       


life :: Context -> Board -> IO ()
life context b                        =  do send context $  
                                                 do (width, height) <- size
                                                    drawGrid context
                                                    clearDeadCells (diff b (nextgen b))
                                                    renderSquares (nextgen b)
                                            threadDelay (50 * 500)
                                            life context (nextgen b) 

drawButton :: Context -> (Float, Float) -> Canvas ()
drawButton context (x,y) = do fillStyle ("red")
                              fillRect (x, y, 30, 20)
                              fillStyle ("white")
                              fillText ("start", x + 5, y + 12.5)
                                            
main = do blankCanvas 3000 { events = ["mousedown"] } $ \ context -> do loop context []
                                                                             
                                                                                