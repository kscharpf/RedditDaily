import Data.List
import System (getArgs)
import System.IO

main :: IO ()
main = do
  args <- getArgs
  outh <- openFile (last args) WriteMode
  hPutStrLn outh (reverse (head args)) 
  hClose outh
