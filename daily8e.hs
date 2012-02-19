buildRefrain :: Int -> String
buildRefrain n = (show n) ++ " bottles of beer on the wall.  " ++ (show n) ++ " bottles of beer. Take one down.  Pass it around.  " ++ (show (n-1)) ++ " bottles of beer on the wall.\n"
ninetyninebottles :: String -> Int -> String
ninetyninebottles xs 0 = xs
ninetyninebottles xs n = ninetyninebottles (xs ++ (buildRefrain n)) (n-1)

main :: IO ()
main = do
  putStrLn (ninetyninebottles [] 99)
