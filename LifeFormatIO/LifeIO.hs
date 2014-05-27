{-# LANGUAGE MultiWayIf #-}
module LifeIO where
    
    import Control.Applicative
    readLife :: FilePath -> IO [(Int, Int)]
    readLife fname = do board <- (dropWhile ((/= "#P") . head) . map words . lines) <$> readFile fname
                        let [_, x, y] = head board
                        let board' = concat $ tail board
                        let xdim = (maximum . map length) board'
                        let ydim = length board'
                        let pairs = [(xcoord, ycoord) | ycoord <- [1..ydim], xcoord <- [1..xdim]] 
                        return $ foldl (\acc arg -> if ((/= '.') . snd) arg
                                                  then (fst arg) : acc
                                                  else acc)
                                       []            
                                       (zip pairs (concat board'))
                                       
    writeLife :: [(Int, Int)] -> FilePath -> IO()
    writeLife board fname = do let mincorner = minimum board
                               let maxcorner = maximum board
                               let xdim = (fst maxcorner) - (fst mincorner) + 1
                               let window = [(xcoord, ycoord) | ycoord <- [1..(snd maxcorner)], xcoord <- [1..xdim]]
                               writeFile fname  ("#Life 1.05 \n#P 1 1\n" ++ (concatMap (\arg -> if | (arg `elem` board) && ((fst arg) `mod` xdim == 0) -> "*\n"
                                                                                                   | arg `elem` board -> "*"
                                                                                                   | (fst arg) `mod` xdim == 0 -> ".\n"
                                                                                                   | otherwise -> ".")
                                                                                      window))
                           
                                           
    
           
                       
                       
    
                        
                        