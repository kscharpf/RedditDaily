import System ( getArgs )

daysToMonth :: Int -> Int
daysToMonth n
  | n == 0 = 0
  | n == 2 = 28 + (daysToMonth 1)
  | ((n == 4) || (n == 6) || (n == 9) || (n == 11)) = 30 + (daysToMonth (n-1))
  | otherwise = 31 + (daysToMonth (n-1))

dayFromDate :: [String] -> Int
dayFromDate [mStr, dStr] = (daysToMonth ((read mStr::Int) - 1)) + (read dStr::Int) 

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ show (dayFromDate args)
  
