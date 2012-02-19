import Data.List.Split

morseCipher = ".... . .-.. .-.. --- / -.. .- .. .-.. -.-- / .--. .-. --- --. .-. .- -- -- . .-. / --. --- --- -.. / .-.. ..- -.-. -.- / --- -. / - .... . / -.-. .... .- .-.. .-.. . -. --. . ... / - --- -.. .- -.--"

morseWords = splitOn " / " morseCipher

morseCharToPlain :: String -> Char
morseCharToPlain str
  | str == ".-" = 'A'
  | str == "-..." = 'B'
  | str == "-.-." = 'C'
  | str == "-.." = 'D'
  | str == "." = 'E'
  | str == "..-." = 'F'
  | str == "--." = 'G'
  | str == "...." = 'H'
  | str == ".." = 'I'
  | str == ".---" = 'J'
  | str == "-.-" = 'K'
  | str == ".-.." = 'L'
  | str == "--" = 'M'
  | str == "-." = 'N'
  | str == "---" = 'O'
  | str == ".--." = 'P'
  | str == "--.-" = 'Q'
  | str == ".-." = 'R'
  | str == "..." = 'S'
  | str == "-" = 'T'
  | str == "..-" = 'U'
  | str == "...-" = 'V'
  | str == ".--" = 'W'
  | str == "-..-" = 'X'
  | str == "-.--" = 'Y'
  | str == "--.." = 'Z'
  | str == ".----" = '1'
  | str == "..---" = '2'
  | str == "...--" = '3'
  | str == "....-" = '4'
  | str == "....." = '5'
  | str == "-...." = '6'
  | str == "--..." = '7'
  | str == "---.." = '8'
  | str == "----." = '9'
  | str == "-----" = '0'
  | otherwise = undefined

morseLetters = map words morseWords

plainLetters = map (map morseCharToPlain) morseLetters

outText = concat (map (++ " ") plainLetters)

main :: IO ()
main = do
  putStrLn outText









