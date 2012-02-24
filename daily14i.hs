sundaramSieveHelper :: [Int] -> Int -> Int -> Int -> [Int]
sundaramSieveHelper xs i j n
  | j + 3 > n = xs
  | i + j + (2*i*j) > n = sundaramSieveHelper xs 1 (j+1) n
  | i > j  = sundaramSieveHelper xs 1 (j+1) n
  | i <= j = sundaramSieveHelper (filter (/= (i + j + (2*i*j))) xs) (i+1) j n 

sundaramSieve :: Int -> [Int]
sundaramSieve n = map (\x -> 2*x + 1) (sundaramSieveHelper (take n [1..]) 1 1 n)
