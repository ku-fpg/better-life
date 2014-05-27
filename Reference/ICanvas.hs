import Graphics.Blank
import Life
import Numeric
import Control.Concurrent



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
                
loop :: Context -> Canvas ()
loop context = do (width, height) <- size
                  sequence_ [lineX x 210 | x <- [10, 20..210]]
                  sequence_ [lineY y 210 | y <- [10, 20..210]]
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

glider                        :: Board
glider                        =  [(4,2),(2,3),(4,3),(3,4),(4,4)]

clearDeadCells :: [(Int, Int)] -> Canvas ()
clearDeadCells xs = sequence_ [clearRect (10 * (fromIntegral x) + 1, 10 * (fromIntegral y) + 1, 8, 8 )| (x,y) <- xs] 


life :: Context -> Board -> IO ()
life context b                        =  do send context $  
                                                 do (width, height) <- size
                                                    loop context
                                                    clearDeadCells (diff b (nextgen b))
                                                    renderSquares (nextgen b)
                                            threadDelay (50 * 500)
                                            life context (nextgen b) 
                                            
main = do blankCanvas 3000 $ \ context -> do life context glider
                                                               