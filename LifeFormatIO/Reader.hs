module Reader where
    
    readLife :: String -> IO [(Int, Int)]
    readLife file = do board <- fmap (map words . lines) $ readFile file
                       return $ map (\[x,y] -> (read x, read y) :: (Int, Int)) $ tail board