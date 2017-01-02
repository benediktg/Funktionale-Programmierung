-- a)
getInt :: IO Int
getInt = getLine
    >>= return.read

-- b)
getNInts :: Int -> IO [Int]
getNInts n
    | n <= 0 = return []
    | otherwise = do i <- getInt
                     is <- getNInts (n - 1)
                     return (i : is)

-- c)
main = do putStrLn "Bitte Anzahl eingeben!"
          n <- getInt
          putStrLn ("Bitte " ++ (show n) ++ " Integer eingeben")
          xs <- getNInts n
          putStrLn ("Die Summe von " ++ show xs ++ " ist " ++ show (sum xs))
