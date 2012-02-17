import Prelude hiding(lookup)
import Data.Map 
import Maybe

validUser :: Map String String -> String -> String -> Bool
validUser m u p = do
    --storedPassword <- lookup u m
    if (isNothing (lookup u m)) 
        then False
        else 
            if (fromJust (lookup u m)) == p
                then True 
                else False
    
--validUser users userName password =

toMap :: [String] -> Map String String -> Map String String
toMap [] ym = ym
toMap (y:ys) ym = toMap ys (insert (head (words y)) (last (words y)) ym)

main :: IO ()
main = do
  putStrLn "Enter user name: "
  userName <- getLine
  putStrLn "Enter password: "
  password <- getLine
  userData <- readFile "users.txt"
  if (validUser (toMap (lines userData) empty) userName password)
      then putStrLn "You have successfully entered the program"
      else putStrLn "Invalid user name and password"
  
