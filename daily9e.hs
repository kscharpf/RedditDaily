import Data.List
import System (getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ show (sort args)
