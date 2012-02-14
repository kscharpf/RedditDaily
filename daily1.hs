main :: IO ()
main =  do
        putStrLn "Enter your name"
        name <- getLine
        putStrLn "How old are you?"
        age <- getLine
        putStrLn "What is your user name?"
        userName <- getLine
        putStrLn ("Your name is " ++ name ++ ", you are " ++ age ++ " years old.  Your user name is " ++ userName)
        
