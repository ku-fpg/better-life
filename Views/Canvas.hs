import Graphics.Blank
import LifeUnwrapped
import Control.Concurrent
import Numeric


renderBall :: (Int,Int)  -> Canvas ()
renderBall (x,y) = do beginPath()
                      let x' = 10 * x
                      let y' = 10 * y
                      fillStyle $ '#' : (concat [showHex (255 - (x' `mod` 255)) "", 
                                                       '0' : (showHex 0 ""),
                                                       --showHex (abs $ (x' `mod` 255) - 255) "", 
                                                       showHex (y' `mod` 255) ""])
                      arc(fromIntegral x', fromIntegral y', 5 , 0, pi*2, False)
                      closePath()
                      fill()
                 
renderBalls :: [(Int, Int)] -> Canvas ()
renderBalls xs   = mapM_ renderBall xs

                      
life :: Context -> Board -> IO ()
life context b                        =  do send context $  
                                                 do (width, height) <- size
                                                    clearRect (0,0, width, height)
                                                    renderBalls b
                                            threadDelay (50 * 50)
                                            life context (nextgen b)                 
                   
glider                        :: Board
glider                        =  [(4,2),(2,3),(4,3),(3,4),(4,4)]

glidergun                     :: Board
glidergun                     = [(2,6), (3,6), (2,7), (3,7), (14,4), (15,4), (17,5), (18,6), (18,7), (18,8), (19,7), (17,9), (16,7), (15,10), (14,10), (13,9),
                                 (12,8), (12,7), (12,6), (13,5), (22,4), (22,5), (22,6), (23, 4), (23,5), (23,6), (24,3), (24,7), (26,2), (26,3), (26,7), (26,8),
                                 (36,4), (36,5), (37,4), (37,5)]


main = do blankCanvas 3000 $ \ context -> do life context glidergun