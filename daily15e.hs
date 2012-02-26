import System ( getArgs )
import Data.Char

leftStripWhitespace :: [Char] -> [Char]
leftStripWhitespace [] = []
leftStripWhitespace (x:xs)
  | isSpace x = leftStripWhitespace xs
  | otherwise = (x:xs)

leftJustifyLine :: [String] -> [String] -> [String]
leftJustifyLine [] ys = ys
leftJustifyLine (x:xs) ys = leftJustifyLine xs (ys ++ [leftStripWhitespace x])

leftJustify :: String -> String
leftJustify xs = unlines $ leftJustifyLine (lines xs) []

main :: IO ()
main = do
    args <- getArgs
    fileData <- readFile $ head args
    writeFile (last args) $ leftJustify fileData
    

