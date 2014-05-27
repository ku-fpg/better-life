import Graphics.Blank
import Life
import Control.Concurrent
import Numeric


renderBall :: (Int,Int)  -> Canvas ()
renderBall (x,y) = do beginPath()
                      let x' = 20 * x
                      let y' = 20 * y
                      fillStyle $ '#' : (concat [showHex (255 - (x' `mod` 255)) "", 
                                                       '0' : (showHex 0 ""),
                                                       --showHex (abs $ (x' `mod` 255) - 255) "", 
                                                       showHex (y' `mod` 255) ""])
                      arc(fromIntegral x', fromIntegral y', 10 , 0, pi*2, False)
                      closePath()
                      fill()
                 
renderBalls :: [(Int, Int)] -> Canvas ()
renderBalls xs   = mapM_ renderBall xs

                      
life :: Context -> Board -> IO ()
life context b                        =  do send context $  
                                                 do (width, height) <- size
                                                    clearRect (0,0, width, height)
                                                    renderBalls b
                                            threadDelay (50 * 500)
                                            life context (nextgen b)                 
                   
glider                        :: Board
glider                        =  [(4,2),(2,3),(4,3),(3,4),(4,4)]

main = do blankCanvas 3000 $ \ context -> do life context glider