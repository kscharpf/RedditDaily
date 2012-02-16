promptMass = "Enter the mass"
promptAccel = "Enter the acceleration"

main :: IO ()
main = prompt

calcForceFromString :: String -> String -> String
calcForceFromString s1 s2 = show ((read s1::Float) * (read s2::Float))

prompt :: IO ()
prompt = do
  putStrLn promptMass
  mStr <- getLine
  putStrLn promptAccel
  aStr <- getLine
  putStrLn ("Force: " ++ (calcForceFromString mStr aStr))
  prompt
  
  
  
