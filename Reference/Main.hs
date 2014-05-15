import Control.Concurrent
import Life

module Main where

    cls                           :: IO ()
    cls                           =  putStr "\ESC[2J"
    
    goto                          :: Pos -> IO ()
    goto (x,y)                    =  putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")
    
    writeat                       :: Pos -> String -> IO ()
    writeat p xs                  =  do goto p
                                        putStr xs
                                        
    showcells                     :: Board -> IO ()
    showcells b                   =  sequence_ [writeat p "O" | p <- b]
    
    life                          :: Board -> IO ()
    life b                        =  do cls
                                        showcells b
                                        threadDelay (50 * 1000)
                                        life (nextgen b)
                                        
    glider                        :: Board
    glider                        =  [(4,2),(2,3),(4,3),(3,4),(4,4)]                                    
                                        
    main = life glider                                    
