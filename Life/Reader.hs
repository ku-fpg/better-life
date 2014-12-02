module Life.Reader where
import Life.Types 

parseLife :: FilePath -> IO [Pos]
parseLife file = do xs <- fmap lines $ readFile file
                    let xs' = drop 1 xs
                    let numcols = length $ head xs'
                    let val = map (\row -> let diff = numcols - (length row)
                                         in if diff /= 0
                                            then row ++ (take diff (cycle "."))
                                            else row)
                                xs' 
                    return $ concatMap (map snd . filter ((=='*') . fst)) $ zipWith (zip) val [[(x,y) | x <- [0..(length (head xs') - 1)]] | y <- [0..(length xs') - 1]]
