import Data.List
import System( getArgs )

main :: IO ()
main = do
  args <- getArgs
  print $ show (permutations (head args))
  
    
