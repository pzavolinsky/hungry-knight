import Tour (getTourIO)

main = do
  mt <- getTourIO (1,5)
  case mt of
    Just p -> (putStrLn $ show $ length p) >> (putStrLn $ show $ p)
    Nothing -> putStrLn "NOT FOUND"
