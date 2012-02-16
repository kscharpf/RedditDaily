import Data.Char

main :: IO ()
main = cipherLoop

encryptHelper :: [Char] -> Int -> [Char] -> [Char]
encryptHelper [] n ys = ys
encryptHelper (x:xs) n ys = encryptHelper xs n (ys ++ [(chr ((( (ord x) - (ord 'a') + n) `mod` 26) + (ord 'a'))   )])

encrypt :: [Char] -> Int -> [Char]
encrypt xs n = encryptHelper xs n []

cipherLoop :: IO ()
cipherLoop = do
    putStrLn "Enter a string to encrypt."
    plainStr <- getLine
    putStrLn "Enter the rotation"
    rotationStr <- getLine 
    putStrLn (encrypt plainStr (read rotationStr::Int))
     
    
