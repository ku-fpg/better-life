module Writer where
        
    writeLife :: String -> [(Int, Int)] -> IO ()
    writeLife fname board = let output = "#Life 1.06 \n" ++ (concatMap (++ " \n") . map (\(x,y) -> show x ++ " " ++ show y)) board
                            in writeFile fname output 